[<AutoOpen>]
module Vide.DSP.DiscreteTime

open System
open Vide

type [<Struct>] DiscreteTimeContext =
    {
        sampleRate: int
        sampleRateFloat: float
        samplePos: int
    }

module DiscreteTimeContext =
    let mkGetCtx sampleRate =
        let ctx = { sampleRate = sampleRate; sampleRateFloat = float sampleRate; samplePos = 0 }
        fun i -> { ctx with samplePos = i }

module Const =
    let pi = Math.PI
    let pi2 = 2.0 * pi
    let sqrt2 = 1.4142135623730951

module Fx =
    module Eval =
        let inline toEvaluable sampleRate ([<InlineIfLambda>] fx) =
            let getCtx = DiscreteTimeContext.mkGetCtx sampleRate
            Fx.Eval.toEvaluable getCtx fx

        let inline toSeq sampleRate ([<InlineIfLambda>] fx) =
            let getCtx = DiscreteTimeContext.mkGetCtx sampleRate
            Fx.Eval.toSeq getCtx fx

    module Envelopes =
        /// An Envelope follower (tc: [0.0 .. 1.0])
        let follower initial tc (input: float) =
            mkVide <| fun s (ctx: DiscreteTimeContext) ->
                let lastValue = s |> Option.defaultValue initial
                let diff = lastValue - input
                let v = lastValue - diff * tc
                v, Some v

        /// An Attack-Release envelope (attackTc, releaseTc: [0.0 .. 1.0])
        let ar initial attackTc releaseTc trigger =
            let targetValue,tc = if trigger then 1.0,attackTc else 0.0,releaseTc
            follower initial tc targetValue 

    module Filter =
        type [<Struct>] BiQuadCoeffs =
            { 
                a0: float
                a1: float
                a2: float
                b1: float
                b2: float
                z1: float
                z2: float
            }

        type [<Struct>] BiQuadParams =
            { 
                q: float
                frq: float
                gain: float 
            }

        let private biQuadCoeffsZero =
            { 
                a0 = 0.0
                a1 = 0.0
                a2 = 0.0
                b1 = 0.0
                b2 = 0.0
                z1 = 0.0
                z2 = 0.0 
            }

        (*
            These implementations are based on http://www.earlevel.com/main/2011/01/02/biquad-formulas/
            and on https://raw.githubusercontent.com/filoe/cscore/master/CSCore/DSP
        *)

        let private biQuadBase (filterParams: BiQuadParams) (calcCoeffs: DiscreteTimeContext -> BiQuadCoeffs) input =
            mkVide <| fun s (ctx: DiscreteTimeContext) ->
                // seed: if we are run the first time, use default values for lastParams+lastCoeffs
                let lastParams, lastCoeffs =
                    match s with
                    | None -> filterParams, calcCoeffs ctx
                    | Some t -> t
                // calc the coeffs new if filter params have changed
                let coeffs =
                    match lastParams = filterParams with
                    | true -> lastCoeffs
                    | false -> calcCoeffs ctx
                let v = input * coeffs.a0 + coeffs.z1
                let z1 = input * coeffs.a1 + coeffs.z2 - coeffs.b1 * v
                let z2 = input * coeffs.a2 - coeffs.b2 * v
                let newCoeffs = { coeffs with z1 = z1; z2 = z2 }
                v, Some (filterParams, newCoeffs)

        let lowPass (p: BiQuadParams) input =
            let calcCoeffs (ctx: DiscreteTimeContext) =
                let k = Math.Tan(Const.pi * p.frq / ctx.sampleRateFloat)
                let norm = 1.0 / (1.0 + k / p.q + k * k)
                let a0 = k * k * norm
                let a1 = 2.0 * a0
                let a2 = a0
                let b1 = 2.0 * (k * k - 1.0) * norm
                let b2 = (1.0 - k / p.q + k * k) * norm
                { biQuadCoeffsZero with
                    a0 = a0
                    a1 = a1
                    a2 = a2
                    b1 = b1
                    b2 = b2 }
            biQuadBase p calcCoeffs input

        let bandPass (p: BiQuadParams) input =
            let calcCoeffs (ctx: DiscreteTimeContext) =
                let k = Math.Tan(Const.pi * p.frq / ctx.sampleRateFloat)
                let norm = 1.0 / (1.0 + k / p.q + k * k)
                let a0 = k / p.q * norm
                let a1 = 0.0
                let a2 = -a0
                let b1 = 2.0 * (k * k - 1.0) * norm
                let b2 = (1.0 - k / p.q + k * k) * norm
                { biQuadCoeffsZero with
                    a0 = a0
                    a1 = a1
                    a2 = a2
                    b1 = b1
                    b2 = b2 }
            biQuadBase p calcCoeffs input

        let highShelf (p: BiQuadParams) input =
            let calcCoeffs (ctx: DiscreteTimeContext) =
                let k = Math.Tan(Const.pi * p.frq / ctx.sampleRateFloat)
                let v = Math.Pow(10.0, Math.Abs(p.gain) / 20.0)
                match p.gain >= 0.0 with
                | true ->
                    // boost
                    let norm = 1.0 / (1.0 + Const.sqrt2 * k + k * k)
                    { biQuadCoeffsZero with
                        a0 = (v + Math.Sqrt(2.0 * v) * k + k * k) * norm
                        a1 = 2.0 * (k * k - v) * norm
                        a2 = (v - Math.Sqrt(2.0 * v) * k + k * k) * norm
                        b1 = 2.0 * (k * k - 1.0) * norm
                        b2 = (1.0 - Const.sqrt2 * k + k * k) * norm }
                | false ->
                    // cut
                    let norm = 1.0 / (v + Math.Sqrt(2.0 * v) * k + k * k)
                    { biQuadCoeffsZero with
                        a0 = (1.0 + Const.sqrt2 * k + k * k) * norm
                        a1 = 2.0 * (k * k - 1.0) * norm
                        a2 = (1.0 - Const.sqrt2 * k + k * k) * norm
                        b1 = 2.0 * (k * k - v) * norm
                        b2 = (v - Math.Sqrt(2.0 * v) * k + k * k) * norm }
            biQuadBase p calcCoeffs input

        let highPass (p: BiQuadParams) input =
            let calcCoeffs (ctx: DiscreteTimeContext) =
                let k = Math.Tan(Const.pi * p.frq / ctx.sampleRateFloat)
                let norm = 1.0 / (1.0 + k / p.q + k * k)
                let a0 = norm
                let a1 = -2.0 * a0
                let a2 = a0
                let b1 = 2.0 * (k * k - 1.0) * norm
                let b2 = (1.0 - k / p.q + k * k) * norm
                { biQuadCoeffsZero with
                    a0 = a0
                    a1 = a1
                    a2 = a2
                    b1 = b1
                    b2 = b2 }
            biQuadBase p calcCoeffs input

        let lowShelf (p: BiQuadParams) input =
            let calcCoeffs (ctx: DiscreteTimeContext) =
                let k = Math.Tan(Const.pi * p.frq / ctx.sampleRateFloat)
                let v = Math.Pow(10.0, Math.Abs(p.gain) / 20.0)
                match p.gain >= 0.0 with
                | true ->
                    // boost
                    let norm = 1.0 / (1.0 + Const.sqrt2 * k + k * k)
                    { biQuadCoeffsZero with
                        a0 = (1.0 + Math.Sqrt(2.0 * v) * k + v * k * k) * norm
                        a1 = 2.0 * (v * k * k - 1.0) * norm
                        a2 = (1.0 - Math.Sqrt(2.0 * v) * k + v * k * k) * norm
                        b1 = 2.0 * (k * k - 1.0) * norm
                        b2 = (1.0 - Const.sqrt2 * k + k * k) * norm }
                | false ->
                    // cut
                    let norm = 1.0 / (1.0 + Math.Sqrt(2.0 * v) * k + v * k * k)
                    { biQuadCoeffsZero with
                        a0 = (1.0 + Const.sqrt2 * k + k * k) * norm
                        a1 = 2.0 * (k * k - 1.0) * norm
                        a2 = (1.0 - Const.sqrt2 * k + k * k) * norm
                        b1 = 2.0 * (v * k * k - 1.0) * norm
                        b2 = (1.0 - Math.Sqrt(2.0 * v) * k + v * k * k) * norm }
            biQuadBase p calcCoeffs input

        let notch (p: BiQuadParams)  input=
            let calcCoeffs (ctx: DiscreteTimeContext) =
                let k = Math.Tan(Const.pi * p.frq / ctx.sampleRateFloat)
                let norm = 1.0 / (1.0 + k / p.q + k * k)
                let a0 = (1.0 + k * k) * norm
                let a1 = 2.0 * (k * k - 1.0) * norm
                let a2 = a0
                let b1 = a1
                let b2 = (1.0 - k / p.q + k * k) * norm
                { biQuadCoeffsZero with
                    a0 = a0
                    a1 = a1
                    a2 = a2
                    b1 = b1
                    b2 = b2 }
            biQuadBase p calcCoeffs input

        let peak (p: BiQuadParams) input =
            let calcCoeffs (ctx: DiscreteTimeContext) =
                let v = Math.Pow(10.0, Math.Abs(p.gain) / 20.0)
                let k = Math.Tan(Const.pi * p.frq / ctx.sampleRateFloat)
                let l = p.q * k + k * k
                match p.gain >= 0.0 with
                | true ->
                    // boost
                    let norm = 1.0 / (1.0 + 1.0 / l)
                    let a1 = 2.0 * (k * k - 1.0) * norm
                    { biQuadCoeffsZero with
                        a0 = (1.0 + v / l) * norm
                        a1 = a1
                        a2 = (1.0 - v / l) * norm
                        b1 = a1
                        b2 = (1.0 - 1.0 / l) * norm }
                | false ->
                    // cut
                    let norm = 1.0 / (1.0 + v / l)
                    let a1 = 2.0 * (k * k - 1.0) * norm
                    { biQuadCoeffsZero with
                        a0 = (1.0 + 1.0 / l) * norm
                        a1 = a1
                        a2 = (1.0 - 1.0 / l) * norm
                        b1 = a1
                        b2 = (1.0 - v / l) * norm }
            biQuadBase p calcCoeffs input

module Gen =
    module Eval =
        let inline toEvaluable sampleRate gen =
            let getCtx = DiscreteTimeContext.mkGetCtx sampleRate
            Gen.Eval.toEvaluable getCtx gen

        let inline toSeq sampleRate gen =
            let getCtx = DiscreteTimeContext.mkGetCtx sampleRate
            Gen.Eval.toSeq getCtx gen

    module Osc =
        // TODO: Don't use the Random dotnet class, but a better random generator for Vide
        let noise =
            mkVide <| fun s (ctx: DiscreteTimeContext) ->
                let random = s |> Option.defaultWith Random
                let v = random.NextDouble()
                v, Some random

        let osc (frq: float) f =
            mkVide <| fun s (ctx: DiscreteTimeContext) ->
                let angle = s |> Option.defaultValue 0.0
                let newAngle = (angle + Const.pi2 * frq / ctx.sampleRateFloat) % Const.pi2
                f newAngle, Some newAngle

        // TODO: phase
        let sin (frq: float) = osc frq Math.Sin
        
        let saw (frq: float) = osc frq (fun angle -> 1.0 - (1.0 / Const.pi * angle))

        let tri (frq: float) =
            osc frq (fun angle ->
                if angle < Const.pi then -1.0 + (2.0 / Const.pi) * angle
                else 3.0 - (2.0 / Const.pi) * angle)

        let square (frq: float) =
            osc frq (fun angle ->
                if angle < Const.pi then 1.0
                else -1.0)
