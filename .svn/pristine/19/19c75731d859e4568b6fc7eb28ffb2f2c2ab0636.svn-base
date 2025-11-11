package eimtest.app.util.net;

import java.util.TimeZone;

import eimtest.app.util.XMLUtil;

/** */
public class EIMHttpServiceCaller {
	/** */
	public HttpClientEIM br;

	/** */
	public XMLUtil xu = new XMLUtil();

	/**
	 * 
	 * @param urlBase
	 * @param userCode
	 * @param userPass
	 * @param lang
	 * @param tz
	 * @throws Exception
	 */
	public EIMHttpServiceCaller(String urlBase) throws Exception {
		br = new HttpClientEIM(urlBase);
	}

	/**
	 * 
	 * @param urlStr
	 * @param params
	 * @return o
	 * @throws Exception
	 */
	public String get(String urlStr, Object[][] params) throws Exception
	{
		return br.get(urlStr, params);
	}

	/**
	 * 
	 * @param userCode
	 * @param password
	 * @param lang
	 * @param tz
	 * @throws Exception
	 */
	public void switchUser(String userCode, String password, String lang, TimeZone tz)
			throws Exception
	{
		preLogin();
		br.eimLogin(userCode, password, lang, tz);
	}

	/**
	 * 
	 * @param userCode
	 * @param password
	 * @throws Exception
	 */
	public void switchUser(String userCode, String password) throws Exception
	{
		switchUser(userCode, password, null, null);
	}

	/**
	 * 
	 * @param userCode
	 * @throws Exception
	 */
	public void switchUser(String userCode) throws Exception
	{
		switchUser(userCode, null);
	}
	
	protected void preLogin() throws Exception {
	}
}
