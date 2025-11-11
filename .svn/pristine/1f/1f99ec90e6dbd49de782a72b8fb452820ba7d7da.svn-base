package eimtest.app.util.net;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.StringReader;
import java.lang.reflect.Method;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import eimtest.app.util.net.SMTPListener.ReceivedMail;

/** */
public class POP3Listener extends SocketListener {
	/** */
	List mailList;

	/** */
	public POP3Listener() {
		this(null);
	}

	/**
	 * 
	 * @param mailList
	 */
	public POP3Listener(List mailList) {
		this.mailList = (mailList != null) ? mailList : new ArrayList();
	}

	protected ListenerThread getListerThread()
	{
		return new POP3LietenerThread();
	}

	/** */
	class POP3LietenerThread extends ListenerThread {
		/** */
		POP3LietenerThread() {
			super(110);
		}

		void process(Socket acceptSock) throws Exception
		{
			String myPOP3ServiceName = "eim.junit.test";
			BufferedReader br = new BufferedReader(new InputStreamReader(acceptSock
					.getInputStream()));
			PrintWriter pw = new PrintWriter(new OutputStreamWriter(acceptSock.getOutputStream()),
					true);
			try
			{
				pw.println("+OK " + myPOP3ServiceName + " pop3 Service Ready");

				String line = readLine(br);
				while (!line.startsWith("USER "))
				{
					pw.println("-ERR not aupported");
					line = readLine(br);
				}
				if (!line.startsWith("USER "))
					throw new IllegalStateException("1.USER message expected.but " + line);
				pw.println("+OK Password required");

				line = readLine(br);
				if (!line.startsWith("PASS "))
					throw new IllegalStateException("2.PASS message expected.but " + line);
				pw.println("+OK");

				// while quit
				while ((line = readLine(br)) != null)
				{
					if (line.trim().length() == 0)
						continue;

					if (line.equals("QUIT"))
					{
						pw.println("+OK Pop server signing off.");
						break;
					}

					line = line + " ";
					String command = line.substring(0, line.indexOf(' '));
					String arg = line.substring(command.length()).trim();
					try
					{
						Method m = this.getClass().getMethod("command" + command.toUpperCase(),
								new Class[] { String.class, PrintWriter.class });
						m.invoke(this, new Object[] { arg, pw });
					} catch (NoSuchMethodException e)
					{
						pw.println("-ERR not supported");
						System.out.println("not supported");
					}
				}

			} catch (Exception e)
			{
				pw.println("-ERR " + e.getMessage());
				e.printStackTrace();
			} finally
			{
				pw.close();
				br.close();
			}
		}

		/**
		 * 
		 * @param br
		 * @return o
		 * @throws IOException
		 */
		String readLine(BufferedReader br) throws IOException
		{
			String line = br.readLine();
			if (doPrintTrace)
				System.out.println(line);
			return line;
		}

		/**
		 * 
		 * @param arg
		 * @param pw
		 */
		public void commandSTAT(String arg, PrintWriter pw)
		{
			pw.println("+OK " + mailList.size() + " " + mailList.toString().length());
		}

		/**
		 * 
		 * @param arg
		 * @param pw
		 */
		public void commandLIST(String arg, PrintWriter pw)
		{
			pw.println("+OK " + mailList.size() + "messages");
			for (int i = 0; i < mailList.size(); i++)
			{
				ReceivedMail mail = (ReceivedMail) mailList.get(i);
				pw.println((i + 1) + " " + mail.plainBody.length());
			}
			pw.println(".");
		}

		/**
		 * 
		 * @param arg
		 * @param pw
		 * @throws IOException
		 */
		public void commandTOP(String arg, PrintWriter pw) throws IOException
		{
			StringTokenizer st = new StringTokenizer(arg, " ");
			String msgNoStr = st.nextToken();
			String linesStr = st.nextToken();
			int msgNo = Integer.parseInt(msgNoStr);
			int lines = Integer.parseInt(linesStr);
			ReceivedMail mail = (ReceivedMail) mailList.get(msgNo - 1);

			pw.println("+OK");
			BufferedReader br = new BufferedReader(new StringReader(mail.plainBody));
			String line;
			while ((line = br.readLine()).length() > 0)
			{
				pw.println(line);
			}
			for (int i = 0; i < lines; i++)
			{
				pw.println(br.readLine());
			}
			pw.println(".");
		}

		/**
		 * 
		 * @param arg
		 * @param pw
		 * @throws IOException
		 */
		public void commandRETR(String arg, PrintWriter pw) throws IOException
		{
			String msgNoStr = arg;
			int msgNo = Integer.parseInt(msgNoStr);
			ReceivedMail mail = (ReceivedMail) mailList.get(msgNo - 1);

			pw.println("+OK");
			pw.println(mail.plainBody);
			pw.println(".");
		}

		/**
		 * 
		 * @param arg
		 * @param pw
		 * @throws IOException
		 */
		public void commandDELE(String arg, PrintWriter pw) throws IOException
		{
			String msgNoStr = arg;
			int msgNo = Integer.parseInt(msgNoStr);
			mailList.remove(msgNo - 1);

			pw.println("+OK");
		}

		/**
		 * 
		 * @param arg
		 * @param pw
		 * @throws IOException
		 */
		public void commandNOOP(String arg, PrintWriter pw) throws IOException
		{
			pw.println("+OK");
		}
	}

}
