package eimtest.app.util.net;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.Stack;

import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpServletResponseWrapper;

/** 
 */
// <filter>
// <filter-name>ResponseSequenceFilter</filter-name>
// <filter-class>eimtest.app.util.ResponseSequenceFilter</filter-class>
// </filter>
// <filter-mapping>
// <filter-name>ResponseSequenceFilter</filter-name>
// <url-pattern>/*</url-pattern>
// </filter-mapping>
public class ResponseSequenceFilter implements Filter {
	/** */
	FilterConfig filterConfig;

	/** */
	int timeToWaitTerm = 300;

	/** */
	int queuePollingTerm = 100;

	/** */
	static Stack queue = new Stack();

	public void destroy()
	{
	}

	public void init(final FilterConfig filterConfig)
	{
		this.filterConfig = filterConfig;
	}

	public void doFilter(final ServletRequest request, final ServletResponse response,
			FilterChain chain) throws IOException, ServletException
	{
		long timeStart = System.currentTimeMillis();
		MyResponseWrapper wrapper = new MyResponseWrapper((HttpServletResponse) response);
		queue.push(wrapper);
		chain.doFilter(request, wrapper);
		long timeToWait = timeToWaitTerm - (System.currentTimeMillis() - timeStart);
		if (timeToWait > 0)
			try
			{
				Thread.sleep(timeToWait);
			} catch (InterruptedException e)
			{
			}
		while (true)
		{
			synchronized (queue)
			{
				if (queue.peek() == wrapper)
				{
					queue.pop();
					OutputStream out = response.getOutputStream();
					out.write(wrapper.getData());
					out.close();
					break;
				}
			}
			try
			{
				Thread.sleep(queuePollingTerm);
			} catch (InterruptedException e)
			{
			}
		}
	}
}

/** */
class MyResponseWrapper extends HttpServletResponseWrapper {
	/** */
	private ByteArrayOutputStream output;

	/** */
	PrintWriter pw;

	/**
	 * 
	 * @param response
	 */
	public MyResponseWrapper(HttpServletResponse response) {
		super(response);
		output = new ByteArrayOutputStream();
	}

	/**
	 * 
	 * @return o
	 */
	public byte[] getData()
	{
		if (pw != null)
			pw.flush();
		return output.toByteArray();
	}

	public ServletOutputStream getOutputStream()
	{
		return new MyServletOutputStream(output);
	}

	public PrintWriter getWriter()
	{
		if (pw == null)
		{
			try
			{
				pw = new PrintWriter(new OutputStreamWriter(output, getCharacterEncoding()));
			} catch (UnsupportedEncodingException e)
			{
				throw new RuntimeException(e);
			}
		}
		return pw;
	}

}

/** */
class MyServletOutputStream extends ServletOutputStream {

	/** */
	private OutputStream os;

	/**
	 * 
	 * @param os
	 */
	public MyServletOutputStream(OutputStream os) {
		this.os = os;
	}

	public void write(int b) throws IOException
	{
		os.write(b);
	}

	public void write(byte[] b) throws IOException
	{
		os.write(b);
	}

	public void write(byte[] b, int off, int len) throws IOException
	{
		os.write(b, off, len);
	}

}
