package eimtest.app.util.net;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.xpath.XPathAPI;
import org.w3c.dom.Node;
import org.w3c.dom.traversal.NodeIterator;
import org.xml.sax.SAXParseException;

import eim.util.EIMConfig;
import eimtest.app.util.XMLUtil;

/** */
public class HttpClient {
	/** */
	static final String CRLF = "\r\n";

	/** */
	Map browserCookie = new HashMap();

	/** */
	String defaultPath;

	/** */
	XMLUtil xu;

	/**
	 * 
	 * @param defaultPath
	 */
	public HttpClient(String defaultPath) {
		this.defaultPath = defaultPath;
		xu = new XMLUtil();
	}

	/**
	 * 
	 * @return o o
	 */
	String getContextUrl() {
		String docUrl = EIMConfig.get("DOCUMENT_URL");
		return docUrl.substring(0, docUrl.indexOf("/app/document") + 1);
	}

	/**
	 * 
	 * @param urlStr
	 * @return o
	 * @throws Exception
	 */
	public String get(String urlStr) throws Exception {
		return get(urlStr, (Map) null);
	}

	/**
	 * 
	 * @param urlStr
	 * @param params
	 * @return o
	 * @throws Exception
	 */
	public String get(String urlStr, Object[][] params) throws Exception {
		Map paramMap = new HashMap();
		if (params != null) {
			for (int i = 0; i < params.length; i++) {
				paramMap.put(params[i][0], params[i][1]);
			}
		}
		return get(urlStr, paramMap);
	}

	/**
	 * 
	 * @param urlStr
	 * @param params
	 * @return o
	 * @throws Exception
	 */
	public String get(String urlStr, Map params) throws Exception {
		boolean isFileUpload = false;
		if (params != null)
			for (Iterator i = params.values().iterator(); i.hasNext();) {
				if (i.next() instanceof File) {
					isFileUpload = true;
					break;
				}
			}

		StringBuffer urlSb = new StringBuffer();
		if (urlStr.startsWith("/"))
			urlSb.append(getContextUrl()).append(urlStr.substring(1));
		else
			urlSb.append(getContextUrl()).append(defaultPath).append("/").append(urlStr);
		String urlPath = urlSb.toString();
		System.out.println(this + " connect to " + urlPath);

		if (!isFileUpload)
			appendParam(urlSb, params);

		URL url = new URL(urlSb.toString());
		HttpURLConnection urlc = (HttpURLConnection) url.openConnection();
		setCookie(urlc);

		if (isFileUpload)
			prepareFileuploadPost(urlc, params);

		InputStream is = null;
		try {
			is = urlc.getInputStream();
		} catch (IOException e) {
			if (!e.getMessage().startsWith("Server returned HTTP response code: ")) throw e;
			String httpCode = e.getMessage().replaceAll(
				"Server returned HTTP response code\\: (\\d*).*", "$1");
			throw new EIMServerResponseError("Http:"
				+ ((httpCode.length() > 0) ? Integer.parseInt(httpCode) : -1));
		}
		StringWriter sw = new StringWriter();
		Reader r = new InputStreamReader(is, "utf-8");
		int c;
		while ((c = r.read()) >= 0) {
			sw.write(c);
		}
		// dumpHeader(urlc);
		if (urlc.getResponseCode() != 200) {
			if (urlc.getResponseCode() == -1) {
				String httpCode = sw.toString().replaceAll(
					".*Cannot find message associated with key http\\.(\\d*).*", "$1");
				throw new EIMServerResponseError("Http:"
						+ ((httpCode.length() > 0) ? Integer.parseInt(httpCode) : -1));
			}

		}
		loadCookie(urlc);
		String ret = sw.toString();
		is.close();

		// error check
		if (ret.replaceAll("\\s", "").length() > 0) {
			NodeIterator nl = null;
			try {
				nl = XPathAPI.selectNodeIterator(xu.toDOM(ret), "/error/@message");
				for (Node n; (n = nl.nextNode()) != null;) {
					throw new EIMServerResponseError(n.getNodeValue());
				}
			} catch (SAXParseException e) {
				System.err.println(this + ":" + e.getMessage());
			}
		}
		return ret;
	}

	/**
	 * 
	 * @param urlc
	 * @param params
	 * @throws Exception
	 */
	void prepareFileuploadPost(HttpURLConnection urlc, Map params) throws Exception {
		String boundaryMain = "!!!!!!------";
		urlc.setDoOutput(true);
		urlc.setRequestMethod("POST");
		urlc.setRequestProperty("Content-Type", "multipart/form-data; boundary=" + boundaryMain);
		OutputStream os = urlc.getOutputStream();
		CRLFPrintWriter pw = new CRLFPrintWriter(new OutputStreamWriter(os, "UTF-8"));
		// write param
		for (Iterator i = params.keySet().iterator(); i.hasNext();) {
			String key = (String) i.next();
			Object val = params.get(key);
			pw.println("--" + boundaryMain);
			if (val instanceof File) {
				File valFile = (File) val;
				pw.println("content-disposition: form-data; name=\"" + key + "\"; filename=\""
						+ encodeUrl(valFile.getName()) + "\"");
				pw.println("Content-Type: application/octet-stream");
				pw.println("");
				BufferedInputStream bis = new BufferedInputStream(new FileInputStream(valFile));
				int c = 0;
				while ((c = bis.read()) != -1) {
					pw.write(c);
				}
				pw.println("");
				bis.close();
			} else {
				pw.println("content-disposition: form-data; name=\"" + key + "\"");
				pw.println("");
				pw.println(val.toString());
			}
		}
		pw.println("--" + boundaryMain + "--");
		pw.flush();
		os.close();
	}

	/**
	 * 
	 * @param sb
	 * @param param
	 */
	void appendParam(StringBuffer sb, Map param) {
		if (param == null)
			return;

		boolean isFirst = true;
		for (Iterator i = param.keySet().iterator(); i.hasNext();) {
			String key = (String) i.next();
			String val = (String) param.get(key);
			sb.append(isFirst ? "?" : "&");
			isFirst = false;
			sb.append(key).append("=").append(encodeUrl(val));
		}
	}

	/**
	 * 
	 * @param str
	 * @return o
	 */
	String encodeUrl(String str) {
		try {
			return URLEncoder.encode(str, "utf-8");
		} catch (UnsupportedEncodingException e) {
			throw new RuntimeException(e);
		}
		// byte[] b;
		// try
		// {
		// b = str.getBytes("utf-8");
		// } catch (UnsupportedEncodingException e)
		// {
		// e.printStackTrace();
		// }
		// StringBuffer sb = new StringBuffer();
		//		
	}

	/**
	 * 
	 * @param urlc
	 */
	void dumpHeader(URLConnection urlc) {
		Map headerMap = urlc.getHeaderFields();
		for (Iterator i = headerMap.keySet().iterator(); i.hasNext();) {
			String key = (String) i.next();
			Object val = headerMap.get(key);
			System.out.println(key + "," + val);
		}
	}

	/**
	 * 
	 * @param urlc
	 */
	void loadCookie(URLConnection urlc) {
		List cookies = (List) urlc.getHeaderFields().get("Set-Cookie");
		if (cookies == null)
			return;
		for (Iterator i = cookies.iterator(); i.hasNext();) {
			String cookie = (String) i.next();
			cookie = cookie.split(";")[0].trim();
			String[] cookieVal = cookie.split("=");
			String name = cookieVal[0].trim();
			String val = cookieVal[1].trim();
			browserCookie.put(name, val);
		}

	}

	/**
	 * 
	 * @param urlc
	 */
	void setCookie(URLConnection urlc) {
		StringBuffer sb = new StringBuffer();
		for (Iterator i = browserCookie.keySet().iterator(); i.hasNext();) {
			String key = (String) i.next();
			String val = (String) browserCookie.get(key);
			if (sb.length() > 0)
				sb.append("; ");
			sb.append(key).append("=").append(val);
		}
		if (sb.length() > 0)
			urlc.addRequestProperty("Cookie", sb.toString());
	}

}
