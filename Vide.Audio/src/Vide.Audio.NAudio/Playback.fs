module Vide.Audio.NAudio

open System.Threading
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

let playSync sampleRate outputChannelCount (duration: TimeSpan) (gen: Vide<_,_,DiscreteTimeContext>) =
    let outSeq = gen |> Gen.Eval.toSeq sampleRate
    let sampleSource = new MonoSampleProvider(sampleRate, outputChannelCount, outSeq)
    use waveOut = new WaveOutEvent()
    do waveOut.Init(sampleSource)
    do waveOut.Play()
    do Thread.Sleep duration
    do waveOut.Stop()
    ()
