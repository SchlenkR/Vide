module Vide.Audio.NAudio

open System.Threading
open System.Threading.Tasks
open Vide
open Vide.Dsp
open System
open NAudio.Wave

type MonoSampleProvider(sampleRate, outputChannelCount, source: float seq) =
    let waveFormat = WaveFormat.CreateIeeeFloatWaveFormat(sampleRate, outputChannelCount)
    let enumerator = source.GetEnumerator()
    interface ISampleProvider with
        member _.WaveFormat = waveFormat
        member _.Read(buffer, offset, count) =
            for i in offset .. ((count - 1) / outputChannelCount) do
                enumerator.MoveNext() |> ignore
                let value = float32 enumerator.Current
                do Array.set buffer (i * outputChannelCount) value
                do Array.set buffer (i * outputChannelCount + 1) value
                ()
            count

module Playback =
    let startAsync (ct: CancellationToken) sampleRate outputChannelCount (duration: TimeSpan) (gen: Vide<_,_,DiscreteTimeContext>) = 
        task {
            let outSeq = gen |> Gen.Eval.toSeq sampleRate
            let sampleSource = new MonoSampleProvider(sampleRate, outputChannelCount, outSeq)
            use waveOut = new WaveOutEvent()
            do waveOut.Init(sampleSource)
            do waveOut.Play()
            do! Task.Delay(duration, ct)
            do waveOut.Stop()
            ()
        }

    let startParallel sampleRate outputChannelCount (duration: TimeSpan) (gen: Vide<_,_,DiscreteTimeContext>) =
        let ct = new CancellationTokenSource()
        let stop = fun () -> ct.Cancel()
        startAsync ct.Token sampleRate outputChannelCount duration gen |> ignore
        {| stop = stop |}

    let startSync sampleRate outputChannelCount (duration: TimeSpan) (gen: Vide<_,_,DiscreteTimeContext>) =
        let task = startAsync CancellationToken.None sampleRate outputChannelCount duration gen
        task.Wait()
