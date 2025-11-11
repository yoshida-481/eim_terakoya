package eimtest.app.util.net;

import java.io.IOException;
import java.io.Writer;

/** */
public class CRLFPrintWriter extends Writer {
	/** */
	static final String CRLF = "\r\n";

	/** */
	Writer w;

	/**
	 * 
	 * @param w
	 */
	public CRLFPrintWriter(Writer w) {
		this.w = w;
	}

	/**
	 * 
	 * @param str
	 * @throws IOException
	 */
	public void println(String str) throws IOException
	{
		w.write(str);
		w.write(CRLF);
	}

	public void close() throws IOException
	{
		flush();
		w.close();
	}

	public void flush() throws IOException
	{
		w.flush();
	}

	public void write(char[] cbuf, int off, int len) throws IOException
	{
		w.write(cbuf, off, len);

	}
}
