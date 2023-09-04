#r "nuget: NAudio"

#I "../Vide.Audio.NAudio/bin/Debug/netstandard2.0"
#r "Vide.Common.dll"
#r "Vide.Audio.NAudio.dll"

open System
open Vide
open Vide.DSP
open Vide.DSP.Operators
open Vide.Audio.NAudio

let modulatedSignal = dsp {
    let! sin1 = Gen.Osc.sin 30.0
    let! sin2 = Gen.Osc.sin 7.0
    return sin1 * sin2
}

let noisyModulated = dsp {
    let! modulated = modulatedSignal
    let! noise = Gen.Osc.noise
    return modulated * noise
}

let lessNoisy = 
    noisyModulated
    |=> Fx.Filter.lowPass { frq = 500.0; q = 0.9; gain = 1.0 }

let voice = Playback.startParallel 44100 2 (TimeSpan.FromSeconds 10.0) lessNoisy
voice.stop()
