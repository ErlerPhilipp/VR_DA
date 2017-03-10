namespace Aardvark.VR

module Logging = 
    open System.IO
    let startupTimeString = System.DateTime.UtcNow.ToLocalTime().ToString("yyyy-MM-dd_HH-mm-ss")
    let appName = System.AppDomain.CurrentDomain.FriendlyName
    System.IO.Directory.CreateDirectory("output") |> ignore
    let sessionFileName = @"output\" + appName + "_" + startupTimeString + ".txt"
    let streamWriter = new StreamWriter(sessionFileName, true)
    streamWriter.AutoFlush <- true
    streamWriter.WriteLine(startupTimeString + ": Start session")

    let log(text : string) =
        streamWriter.WriteLine(text)
        printfn "%A" text

    let endSession() =
        let endTimeString = System.DateTime.UtcNow.ToLocalTime().ToString("yyyy-MM-dd_HH-mm-ss")
        streamWriter.WriteLine(endTimeString + ": End session")
        streamWriter.Flush() // doesn't do anything?
        streamWriter.Dispose()