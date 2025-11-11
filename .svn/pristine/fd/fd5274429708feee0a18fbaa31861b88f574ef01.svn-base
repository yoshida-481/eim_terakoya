package eimtest.app.tool;

import java.util.ArrayList;
import java.util.Arrays;

import eimtest.app.util.net.POP3Listener;
import eimtest.app.util.net.SocketListener;
import eimtest.app.util.net.SMTPListener.ReceivedMail;

/** */
public class POP3Service {
	/**
	 * 
	 * @param args
	 * @throws Exception
	 */
	public static void main(String[] args) throws Exception
	{
		SocketListener smtpListener = new POP3Listener(new ArrayList(Arrays.asList(new Object[] {
				new ReceivedMail(null, "abc","abc")//
				, new ReceivedMail(null, "abc","abc") //
				}))).start().setDoPrintTrace(true);
		while (smtpListener.isInService())
		{
			Thread.sleep(1000);
		}
	}
}
