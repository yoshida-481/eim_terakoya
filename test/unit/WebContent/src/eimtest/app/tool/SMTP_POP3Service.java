package eimtest.app.tool;

import java.util.Date;
import java.util.Properties;
import java.util.Stack;

import javax.activation.DataHandler;
import javax.activation.FileDataSource;
import javax.mail.Message;
import javax.mail.Multipart;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;
import javax.mail.internet.MimeUtility;

import org.apache.commons.lang.ClassUtils;

import eimtest.app.util.net.POP3Listener;
import eimtest.app.util.net.SMTPListener;

/** */
public class SMTP_POP3Service extends Thread {
	/** */
	SMTPListener smtp;

	/** */
	POP3Listener pop3;

	/** */
	Stack mails;

	/**
	 * 
	 * @param args
	 */
	public static void main(String[] args)
	{
		new SMTP_POP3Service(args).start();
	}

	/**
	 * 
	 * @param args
	 */
	SMTP_POP3Service(String[] args) {
		mails = new Stack();
		smtp = new SMTPListener(mails);
		smtp.setDoPrintTrace(true);
		smtp.start();
		pop3 = new POP3Listener(mails);
		pop3.setDoPrintTrace(true);
		pop3.start();
		send();
	}

	public void run()
	{
		while (true)
		{
			try
			{
				Thread.sleep(500);
			} catch (InterruptedException e)
			{
				break;
			}
		}
		smtp.terminate();
		pop3.terminate();
	}

	/** */
	void send()
	{
		try
		{
			Properties props = System.getProperties();
			// SMTPサーバーのアドレスを指定
			props.put("mail.smtp.host", "localhost");
			Session session = Session.getDefaultInstance(props, null);
			MimeMessage mimeMessage = new MimeMessage(session);
			// 送信元メールアドレスと送信者名を指定
			mimeMessage.setFrom(new InternetAddress("u2@eim.com", "u2", "iso-2022-jp"));
			// 送信先メールアドレスを指定
			mimeMessage.setRecipients(Message.RecipientType.TO, "u1@eim.com");
			// メールのタイトルを指定
			mimeMessage.setSubject("SendMailでファイル添付！", "iso-2022-jp");

			/** １つ目のボディパートを作成 * */
			MimeBodyPart mbp1 = new MimeBodyPart();
			// メールの内容を指定
			mbp1.setText("SendMailでファイルを添付します。", "iso-2022-jp");

			/** ２つ目のボディパートを作成 * */
			MimeBodyPart mbp2 = new MimeBodyPart();
			// 添付するファイル名を指定
			FileDataSource fds = new FileDataSource(this.getClass().getResource(
					ClassUtils.getShortClassName(this.getClass()) + ".class").getPath());
			mbp2.setDataHandler(new DataHandler(fds));
			mbp2.setFileName(MimeUtility.encodeWord(fds.getName()));

			// 複数のボディを格納するマルチパートオブジェクトを生成
			Multipart mp = new MimeMultipart();
			// １つ目のボディパートを追加
			mp.addBodyPart(mbp1);
			// ２つ目のボディパートを追加
			mp.addBodyPart(mbp2);

			// マルチパートオブジェクトをメッセージに設定
			mimeMessage.setContent(mp);

			// 送信日付を指定
			mimeMessage.setSentDate(new Date());
			// 送信します
			Transport.send(mimeMessage);
		} catch (Exception e)
		{
			throw new RuntimeException(e);
		}
	}

}
