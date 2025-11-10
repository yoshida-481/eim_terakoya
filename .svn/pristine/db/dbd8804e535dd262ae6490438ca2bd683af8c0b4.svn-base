package eimtest.app.util.net;

import java.util.TimeZone;

import org.w3c.dom.Document;

/** */
public class HttpClientEIM extends HttpClient {
	/**
	 * 
	 * @param defaultPath
	 */
	public HttpClientEIM(String defaultPath) {
		super(defaultPath);
	}

	/**
	 * 
	 * @param userCode
	 * @param userPass
	 * @param lang
	 * @param tz
	 * @return o
	 * @throws Exception
	 */
	public Document eimLogin(String userCode, String userPass, String lang, TimeZone tz)
			throws Exception
	{
		if (userPass == null)
			userPass = userCode;
		if (tz == null)
			tz = TimeZone.getDefault();
		if (lang == null)
			lang = "JA";

		String ret = get("session/actLogin.jsp", new String[][] {//
				{ "userCode", userCode }//
						, { "userPass", userPass }//
						, { "langId", lang } //
						, { "userTzOffset", String.valueOf(-tz.getRawOffset()) } //
				});
		return xu.toDOM(ret);
	}

	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	public Document eimLogout() throws Exception
	{
		String ret = get("session/actLogout.jsp");
		return xu.toDOM(ret);
	}
}
