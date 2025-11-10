package eimtest.app.tool;

import eimtest.app.util.net.SMTPListener;
import eimtest.app.util.net.SocketListener;

/** */
public class SMTPService {
	/**
	 * 
	 * @param args
	 * @throws Exception
	 */
	public static void main(String[] args) throws Exception
	{
		SocketListener smtpListener = new SMTPListener().start().setDoPrintTrace(true);
		while (smtpListener.isInService())
		{
			Thread.sleep(1000);
		}
	}
}
