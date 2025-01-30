' Usage: cscript watch_files.vbs <database_path>
'
' Watches files according to configuration in SQLite database and logs events

Option Explicit

' Check arguments
If WScript.Arguments.Count < 1 Then
    WScript.StdErr.WriteLine "Missing database path"
    WScript.Quit 1
End If

Dim dbPath: dbPath = WScript.Arguments(0)

' Verify database exists
Dim fso: Set fso = CreateObject("Scripting.FileSystemObject")
If Not fso.FileExists(dbPath) Then
    WScript.StdErr.WriteLine "Database not found: " & dbPath
    WScript.Quit 1
End If

' Setup database access - try ODBC first, fall back to R wrapper
Dim useODBC: useODBC = True
Dim conn: Set conn = Nothing

Function InitializeDatabase()
    On Error Resume Next
    Set conn = CreateObject("ADODB.Connection")
    conn.Open "Driver={SQLite3 ODBC Driver};Database=" & dbPath & ";"

    If Err.Number <> 0 Then
        ' Try alternate driver name
        Err.Clear
        conn.Open "Driver={SQLite ODBC Driver};Database=" & dbPath & ";"
    End If

    If Err.Number <> 0 Then
        useODBC = False
        Set conn = Nothing
    End If
    On Error Goto 0
End Function

' Function to execute SQL using either ODBC or R wrapper
Function ExecuteSQL(sql)
    If useODBC Then
        Dim rs: Set rs = conn.Execute(sql)
        If Not rs.EOF Then
            ExecuteSQL = rs.Fields(0).Value
        Else
            ExecuteSQL = ""
        End If
    Else
        ' Use R wrapper script
        Dim shell: Set shell = CreateObject("WScript.Shell")
        Dim cmd: cmd = "Rscript sqlite-wrapper.R """ & dbPath & """ """ & _
                      Replace(sql, """", "\""") & """"
        Dim exec: Set exec = shell.Exec(cmd)
        ExecuteSQL = exec.StdOut.ReadAll
    End If
End Function

' Initialize database
InitializeDatabase

' Read configuration
Function ReadConfig(key)
    ReadConfig = ExecuteSQL("SELECT value FROM config WHERE key='" & key & "';")
End Function

Dim watchPath: watchPath = ReadConfig("path")
Dim pattern: pattern = ReadConfig("pattern")
Dim recursive: recursive = (ReadConfig("recursive") = "true")
Dim watchMode: watchMode = ReadConfig("watch_mode")
Dim changeType: changeType = ReadConfig("change_type")
Dim callbackScript: callbackScript = ReadConfig("callback_script")

If Len(watchPath) = 0 Then
    WScript.StdErr.WriteLine "Invalid configuration: path not found"
    WScript.Quit 1
End If

' Write our PID to database status
ExecuteSQL "INSERT OR REPLACE INTO status (key, value) VALUES ('pid', '" & _
          CreateObject("WScript.Shell").Exec("cmd /c echo %pid%").StdOut.ReadAll & "');"

' Create FileSystemWatcher
Dim watcher: Set watcher = CreateObject("System.IO.FileSystemWatcher")
With watcher
    .Path = watchPath
    If Len(pattern) > 0 Then .Filter = pattern
    .IncludeSubdirectories = recursive
    .NotifyFilter = 17 ' LastWrite + FileName + DirectoryName
End With

' Function to write event to database
Sub WriteEvent(eventType, filePath)
    ' Only process events matching configured type
    If changeType <> "any" And changeType <> eventType Then
        Exit Sub
    End If

    ' Insert event
    Dim timestamp: timestamp = Year(Now) & "-" & Right("0" & Month(Now), 2) & "-" & _
                   Right("0" & Day(Now), 2) & " " & Right("0" & Hour(Now), 2) & ":" & _
                   Right("0" & Minute(Now), 2) & ":" & Right("0" & Second(Now), 2)

    Dim sql: sql = "INSERT INTO events (timestamp, event_type, file_path) " & _
                   "VALUES ('" & timestamp & "', '" & eventType & "', '" & _
                   Replace(filePath, "'", "''") & "');"

    ExecuteSQL sql

    ' Launch callback if configured
    If Len(callbackScript) > 0 Then
        Dim eventId: eventId = ExecuteSQL("SELECT last_insert_rowid();")
        If Len(eventId) > 0 Then
            CreateObject("WScript.Shell").Run _
            "Rscript -e ""library(vigil); vigil:::execute_callback('" & _
            Replace(dbPath, "'", "''") & "', " & eventId & ")""", 0, False
        End If
    End If

    ' Exit after first event in single mode
    If watchMode = "single" Then
        cleanup
        WScript.Quit 0
    End If
End Sub

' Cleanup function
Sub cleanup()
    On Error Resume Next
    ExecuteSQL "INSERT OR REPLACE INTO status (key, value) VALUES ('state', 'stopped');"
    If Not conn Is Nothing Then conn.Close
End Sub

' Set up cleanup on script termination
Sub OnTerminate
    cleanup
End Sub

' Event handlers for different file changes
Sub OnCreated(obj, ev)
    WriteEvent "created", ev.FullPath
End Sub

Sub OnChanged(obj, ev)
    WriteEvent "modified", ev.FullPath
End Sub

Sub OnDeleted(obj, ev)
    WriteEvent "deleted", ev.FullPath
End Sub

' Connect event handlers
watcher.OnCreated = GetRef("OnCreated")
watcher.OnChanged = GetRef("OnChanged")
watcher.OnDeleted = GetRef("OnDeleted")

' Mark watcher as running
ExecuteSQL "INSERT OR REPLACE INTO status (key, value) VALUES ('state', 'running');"

' Enable watching
watcher.EnableRaisingEvents = True

' Main loop - keep script running
Do While True
    WScript.Sleep 1000

    ' Check if our parent process is still alive
    If WScript.StdIn.AtEndOfStream Then
        cleanup
        WScript.Quit 0
    End If
Loop
