/*---------------------------------------------------------------------------*/
/*                                Run process                                */
/*---------------------------------------------------------------------------*/
import java::io::*
import java::lang::Runtime
import java::lang::StringBuilder

Def Read(inputStream: InputStream) =
	val reader = new BufferedReader(new InputStreamReader(inputStream))
	var line: String? = null
	while ( (line = reader.readLine()) != null)
		println(line)

val proc = Runtime.getRuntime().exec("whoami")
Read(proc.getInputStream())
Read(proc.getErrorStream())
