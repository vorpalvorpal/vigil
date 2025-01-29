' Usage: cscript callback-runner.vbs <database_path> <event_id> <callback_script>
'
' Runs the callback script for a specific event and records the output

Option Explicit

' Check arguments
If WScript.Arguments.Count < 3 Then
    WScript.StdErr.WriteLine "Invalid arguments"
    WScript.Quit 1
End If

Dim dbPath: dbPath = WScript.Arguments(0)
Dim eventId: eventId = WScript.Arguments(1)
Dim callbackScript: callbackScript = WScript.Arguments(2)

' Verify files exist
Dim fso: Set fso = CreateObject("Scripting.FileSystemObject")
If Not (fso.FileExists(dbPath) And fso.FileExists(callbackScript)) Then
    WScript.StdErr.WriteLine "Required files not found"
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

' Create temporary file for output
Dim tempPath: tempPath = fso.GetSpecialFolder(2) & "\" & fso.GetTempName()

' Run the callback in R and capture output
Dim shell: Set shell = CreateObject("WScript.Shell")
Dim rCmd: rCmd = "Rscript -e ""tryCatch({ " & _
                 "event <- DBI::dbGetQuery(" & _
                 "DBI::dbConnect(RSQLite::SQLite(), '" & Replace(dbPath, "'", "''") & "'), " & _
                 "'SELECT * FROM events WHERE id = " & eventId & "'); " & _
                 "source('" & Replace(callbackScript, "'", "''") & "') " & _
                 "}, error = function(e) { " & _
                 "cat('Error in callback: ', conditionMessage(e), '\n') " & _
                 "})"" > """ & tempPath & """ 2>&1"

shell.Run rCmd, 0, True

' Read output and escape for SQL
Dim output: output = ""
If fso.FileExists(tempPath) Then
    Dim textStream: Set textStream = fso.OpenTextFile(tempPath, 1)
    output = Replace(textStream.ReadAll(), "'", "''")
    textStream.Close
    fso.DeleteFile tempPath
End If

' Update event with callback output and timestamp
Dim timestamp: timestamp = Year(Now) & "-" & Right("0" & Month(Now), 2) & "-" & _
               Right("0" & Day(Now), 2) & " " & Right("0" & Hour(Now), 2) & ":" & _
               Right("0" & Minute(Now), 2) & ":" & Right("0" & Second(Now), 2)

ExecuteSQL "UPDATE events " & _
          "SET callback_output = '" & output & "', " & _
          "    callback_timestamp = '" & timestamp & "' " & _
          "WHERE id = " & eventId

' Clean up
If useODBC And Not conn Is Nothing Then conn.Close
