package eimtest.app.util.net;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringWriter;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

import javax.mail.internet.MimeUtility;

import org.apache.tools.ant.filters.StringInputStream;

/** */
public class SMTPListener extends SocketListener {
	/** */
	Stack recievedMailQueue = new Stack();

	/** */
	public SMTPListener() {
		this(null);
	}

	/**
	 * 
	 * @param recievedMailQueue
	 */
	public SMTPListener(Stack recievedMailQueue) {
		this.recievedMailQueue = (recievedMailQueue != null) ? recievedMailQueue : new Stack();
	}

	protected ListenerThread getListerThread()
	{
		return new SMTPLietenerThread();
	}

	/** */
	public void resetQueue()
	{
		synchronized (recievedMailQueue)
		{
			recievedMailQueue.clear();
		}
	}

	/**
	 * 
	 * @return o
	 */
	public List getRecievedQueue()
	{
		synchronized (recievedMailQueue)
		{
			return (List) recievedMailQueue.clone();
		}
	}

	/**
	 * 
	 * @return o
	 */
	public ReceivedMail popQueue()
	{
		synchronized (recievedMailQueue)
		{
			if (recievedMailQueue.size() == 0)
				return null;
			return (ReceivedMail) recievedMailQueue.pop();
		}
	}

	/** */
	class SMTPLietenerThread extends ListenerThread {
		/** */
		SMTPLietenerThread() {
			super(25);
		}

		void process(Socket acceptSock) throws Exception
		{
			String mySmtpServiceName = "eim.junit.test";
			BufferedReader br = new BufferedReader(new InputStreamReader(acceptSock
					.getInputStream()));
			PrintWriter pw = new PrintWriter(new OutputStreamWriter(acceptSock.getOutputStream()),
					true);
			try
			{
				pw.println("220 " + mySmtpServiceName + " Simple Mail Transfer Service Ready");

				String line = br.readLine();
				if (doPrintTrace)
					System.out.println(line);
				if (!line.startsWith("EHLO "))
					throw new IllegalStateException("0.EHLO message expected.but " + line);
				pw.println("250 ok");

				line = br.readLine();
				if (doPrintTrace)
					System.out.println(line);
				if (!line.startsWith("MAIL FROM:"))
					throw new IllegalStateException("1.MAIL message expected.but " + line);
				pw.println("250 ok");

				List reciptTos = new ArrayList();
				while ((line = br.readLine()).startsWith("RCPT TO:"))
				{
					if (doPrintTrace)
						System.out.println(line);
					pw.println("250 ok");
					reciptTos.add(line.substring(9).trim());
				}
				if (doPrintTrace)
					System.out.println(line);
				if (reciptTos.size() == 0)
				{
					throw new IllegalStateException("2.RCPT message expected.but none");
				}

				if (!line.equals("DATA"))
					throw new IllegalStateException("3.DATA message expected.but " + line);
				pw.println("354 Start mail input; end with <CRLF>.<CRLF>");

				StringWriter sw = new StringWriter();
				PrintWriter bodyPw = new PrintWriter(sw);
				while (!(line = br.readLine()).equals("."))
				{
					if (doPrintTrace)
						System.out.println(line);
					bodyPw.println(line);
				}
				if (doPrintTrace)
					System.out.println(line);
				bodyPw.close();
				pw.println("250 ok");

				line = br.readLine();
				if (!line.equals("QUIT"))
					throw new IllegalStateException("4.QUIT message expected.but " + line);
				pw.println("221 " + mySmtpServiceName + " Service closing transmission channel");

				String plainBody = sw.toString();
				Reader r = new InputStreamReader(MimeUtility.decode(
						new StringInputStream(plainBody), "quoted-printable"), "iso-2022-jp");
				sw = new StringWriter();
				int c;
				while ((c = r.read()) >= 0)
					sw.write(c);
				String decodedBody = sw.toString();
				if (doPrintTrace)
				{
					System.out.println("-------decoded-------");
					System.out.println(decodedBody);
				}
				recievedMailQueue.push(new ReceivedMail(reciptTos, decodedBody, plainBody));

			} catch (Exception e)
			{
				pw.println("500 " + e.getMessage());
				e.printStackTrace();
			} finally
			{
				pw.close();
				br.close();
			}
		}
	}

	/** */
	public static class ReceivedMail {
		/** */
		public List reciptTos;

		/** */
		public String body;

		/**
		 * 
		 */
		public String plainBody;

		/**
		 * 
		 * @param reciptTos
		 * @param body
		 * @param plainBody
		 */
		public ReceivedMail(List reciptTos, String body, String plainBody) {
			this.reciptTos = reciptTos;
			this.body = body;
			this.plainBody = plainBody;
		}
	}
}
