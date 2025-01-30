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

' Flag for ODBC vs R wrapper usage
Dim useODBC: useODBC = True

' Test database connection method
Function TestDatabaseAccess()
    On Error Resume Next
    Dim testConn: Set testConn = CreateObject("ADODB.Connection")
    testConn.Open "Driver={SQLite3 ODBC Driver};Database=" & dbPath & ";"

    If Err.Number <> 0 Then
        ' Try alternate driver name
        Err.Clear
        testConn.Open "Driver={SQLite ODBC Driver};Database=" & dbPath & ";"
    End If

    If Err.Number <> 0 Then
        useODBC = False
    Else
        testConn.Close
    End If
    Set testConn = Nothing
    On Error Goto 0
End Function

' Function to execute SQL using either ODBC or R wrapper
Function ExecuteSQL(sql)
    ' First check if database still exists
    If Not fso.FileExists(dbPath) Then
        ExecuteSQL = ""
        Exit Function
    End If

    If useODBC Then
        ' Create new connection
        Dim tempConn: Set tempConn = CreateObject("ADODB.Connection")
        On Error Resume Next

        ' Try primary driver
        tempConn.Open "Driver={SQLite3 ODBC Driver};Database=" & dbPath & ";"
        If Err.Number <> 0 Then
            ' Try alternate driver
            Err.Clear
            tempConn.Open "Driver={SQLite ODBC Driver};Database=" & dbPath & ";"
        End If

        If Err.Number <> 0 Then
            ' Fall back to R wrapper
            useODBC = False
            Set tempConn = Nothing
            ExecuteSQL = ExecuteSQL(sql)  ' Recursive call will use R wrapper
            Exit Function
        End If
        On Error Goto 0

        ' Set timeout and begin transaction
        tempConn.Execute "PRAGMA busy_timeout=5000;"

        ' Execute query
        Dim result
        Dim rs: Set rs = tempConn.Execute(sql)
        If Not rs.EOF Then
            result = rs.Fields(0).Value
        Else
            result = ""
        End If

        ' Clean up
        rs.Close
        Set rs = Nothing
        tempConn.Close
        Set tempConn = Nothing

        ExecuteSQL = result
    Else
        ' Use R wrapper script
        Dim shell: Set shell = CreateObject("WScript.Shell")
        Dim cmd: cmd = "Rscript sqlite-wrapper.R """ & dbPath & """ """ & _
                      Replace(sql, """", "\""") & """"
        Dim exec: Set exec = shell.Exec(cmd)
        ExecuteSQL = exec.StdOut.ReadAll
    End If
End Function

' Function to get current process PID
Function GetCurrentPID()
    Dim shell: Set shell = CreateObject("WScript.Shell")
    Dim exec: Set exec = shell.Exec("cmd /c echo %pid%")
    GetCurrentPID = CInt(exec.StdOut.ReadAll)
End Function

' Read configuration from database
Function ReadConfig(key)
    ReadConfig = ExecuteSQL("SELECT value FROM config WHERE key='" & key & "';")
End Function

' Initialize database access method
TestDatabaseAccess

' Read configuration
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

' Write our PID to active_processes
Dim currentPid: currentPid = GetCurrentPID()
ExecuteSQL "INSERT INTO active_processes (pid, type) VALUES (" & currentPid & ", 'watcher');"

' Function to write event to database and launch callback
Sub WriteEvent(eventType, filePath)
    ' Only process events matching configured type
    If changeType <> "any" And changeType <> eventType Then
        Exit Sub
    End If

    ' Get timestamp
    Dim timestamp: timestamp = Year(Now) & "-" & Right("0" & Month(Now), 2) & "-" & _
                   Right("0" & Day(Now), 2) & " " & Right("0" & Hour(Now), 2) & ":" & _
                   Right("0" & Minute(Now), 2) & ":" & Right("0" & Second(Now), 2)

    ' Begin transaction
    Dim sql: sql = "BEGIN TRANSACTION;" & _
                   "INSERT INTO events (timestamp, event_type, file_path) " & _
                   "VALUES ('" & timestamp & "', '" & eventType & "', '" & _
                   Replace(filePath, "'", "''") & "');" & _
                   "COMMIT;"

    ExecuteSQL sql

    ' Launch callback if configured
    If Len(callbackScript) > 0 Then
        Dim eventId: eventId = ExecuteSQL("SELECT last_insert_rowid();")
        If Len(eventId) > 0 Then
            ' Launch callback wrapper
            Dim shell: Set shell = CreateObject("WScript.Shell")
            shell.Run "cscript //NoLogo callback_wrapper.vbs """ & _
                     dbPath & """ " & eventId, 0, False
        End If
    End If

    ' Exit after first event in single mode
    If watchMode = "single" Then
        WScript.Quit 0
    End If
End Sub

' Create FileSystemWatcher
Dim watcher: Set watcher = CreateObject("System.IO.FileSystemWatcher")
With watcher
    .Path = watchPath
    If Len(pattern) > 0 Then .Filter = pattern
    .IncludeSubdirectories = recursive
    .NotifyFilter = 17 ' LastWrite + FileName + DirectoryName
End With

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

' Enable watching
watcher.EnableRaisingEvents = True

' Main loop - keep script running
Do While True
    WScript.Sleep 1000

    ' Check if our parent process is still alive
    If WScript.StdIn.AtEndOfStream Then
        WScript.Quit 0
    End If
Loop
