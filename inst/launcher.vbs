' launcher.vbs
'
' Usage: cscript launcher.vbs <database_path> <watcher_script_path>
'
' Returns: PID of the watcher process once it's confirmed running

Option Explicit

' Check arguments
If WScript.Arguments.Count < 2 Then
    WScript.StdErr.WriteLine "Invalid arguments"
    WScript.Quit 1
End If

Dim dbPath, watcherScript
dbPath = WScript.Arguments(0)
watcherScript = WScript.Arguments(1)

' Verify files exist
Dim fso: Set fso = CreateObject("Scripting.FileSystemObject")
If Not (fso.FileExists(dbPath) And fso.FileExists(watcherScript)) Then
    WScript.StdErr.WriteLine "Required files not found"
    WScript.Quit 1
End If

' Create auto-reset event for synchronization
Dim syncEvent: Set syncEvent = CreateObject("Scripting.Dictionary")
syncEvent.Add "signaled", False

' Create FileSystemWatcher for database
Dim watcher: Set watcher = CreateObject("System.IO.FileSystemWatcher")
With watcher
    .Path = fso.GetParentFolderName(dbPath)
    .Filter = fso.GetFileName(dbPath)
    .NotifyFilter = 16  ' LastWrite
End With

' Event handler that signals our sync event
Sub OnModified(o, e)
    syncEvent("signaled") = True
End Sub

' Connect event handler
watcher.OnModified = GetRef("OnModified")

' Launch watcher script before enabling events
Dim shell: Set shell = CreateObject("WScript.Shell")
Dim proc: Set proc = shell.Exec("cscript //NoLogo " & watcherScript & " " & dbPath)
Dim watcherPid: watcherPid = proc.ProcessID

' Enable watching
watcher.EnableRaisingEvents = True

' Wait for modification event with timeout
Dim startTime: startTime = Timer()
watcher.WaitForChanged(16, 10000) ' Wait up to 10 seconds for LastWrite

' Check if we got signaled within timeout
If Not syncEvent("signaled") Then
    ' Timeout - kill watcher and exit
    shell.Run "taskkill /F /PID " & watcherPid, 0, True
    WScript.StdErr.WriteLine "Watcher failed to start (timeout)"
    WScript.Quit 1
End If

' Check if PID was written to database
Dim conn: Set conn = CreateObject("ADODB.Connection")
conn.Open "Driver={SQLite3 ODBC Driver};Database=" & dbPath & ";"

Dim rs: Set rs = conn.Execute("SELECT pid FROM active_processes WHERE type='watcher' AND active=TRUE LIMIT 1;")
If Not rs.EOF Then
    ' Success - output PID and exit
    WScript.StdOut.WriteLine watcherPid
    WScript.Quit 0
End If

' If we get here, database was modified but PID wasn't written
shell.Run "taskkill /F /PID " & watcherPid, 0, True
WScript.StdErr.WriteLine "Watcher failed to start (no PID written)"
WScript.Quit 1
