$psArray = [System.Diagnostics.Process]::GetProcesses()
foreach ($ps in $psArray){  
    if($ps.MainWindowTitle -match $Args[0]){
      [String]$ps.MainWindowTitle
      exit -1
    }
}