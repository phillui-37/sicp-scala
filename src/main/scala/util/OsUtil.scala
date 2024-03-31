package util

import java.io.{BufferedReader, InputStreamReader}
import java.text.SimpleDateFormat
import java.util.regex.Pattern

def getSystemUptime: Long =
  var time = -1L
  val os = System.getProperty("os.name").toLowerCase()
  if os contains "win" then {
    val upTimeProc = Runtime.getRuntime exec "net stat srv"
    BufferedReader(InputStreamReader(upTimeProc.getInputStream))
      .lines
      .filter(_ startsWith "Statistics since")
      .findFirst
      .map(x => {
        val format = SimpleDateFormat("'Statistics since' MM/dd/yyyy hh:mm:ss a")
        val boottime = format parse x
        time = System.currentTimeMillis - boottime.getTime
        x
      })
  } else if !("mac,nix,nux,aix" split "," exists (os contains _)) then {
    val upTimeProc = Runtime.getRuntime exec "uptime"
    val line =BufferedReader(InputStreamReader(upTimeProc.getInputStream))
      .readLine
    if line != null then {
      val pattern = Pattern compile "((\\\\d+) days,)? (\\\\d+):(\\\\d+)"
      val matcher = pattern matcher line
      if matcher.find then {
        val sd = matcher group 2
        val sh = matcher group 3
        val sm = matcher group 4
        val d = if sd != null then Integer parseInt sd else 0
        val h = if sh != null then Integer parseInt sh else 0
        val m = if sm != null then Integer parseInt sm else 0
        time = m * 60000 + h * 60000 * 60 + d * 60000 * 60 * 24
      }
    }
  }
  time