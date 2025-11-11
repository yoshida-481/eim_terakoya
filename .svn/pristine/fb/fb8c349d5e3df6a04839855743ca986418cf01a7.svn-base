package jp.co.ctc_g.eim.app.document.presentation.servlet;

import java.io.IOException;
import java.io.PrintWriter;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.commons.text.StringEscapeUtils;


/**
 * HTMLにてメッセージをアラート表示するサーブレットクラスです。
 * <p>
 * このクラスは
 * <code>{@link jakarta.servlet.http.HttpServlet}</code>
 * を拡張している。
 *
 */
public class DocumentHtmlErrorMessageServlet extends HttpServlet {

	/** シリアルバージョンUID */
	private static final long serialVersionUID = 1L;

	/** エラーメッセージ定義 */
	public final String ERROR_ATTR_KEY = "errorMessage";

	/** 出力ログ */
	Log log = LogFactory.getLog(this.getClass().getName());

	/**
	 * GET処理<br>
	 * doPostを呼び出します。
	 * @param request HttpServletRequest
	 * @param response HttpServletResponse
	 *
	 * @throws IOException
	 * @throws ServletException
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
		doPost(request, response);
	}

	/**
	 * POST処理<br>
	 * createCsvを呼び出します。
	 * @param request
	 * @param response
	 *
	 * @throws IOException
	 * @throws ServletException
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {

		Log log = LogFactory.getLog(this.getClass().getName());

		//ContentType
		response.setContentType("text/html; charset=UTF-8");
		response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	    // 出力用PrintWriterを取得
	    PrintWriter out = response.getWriter();

		String message = StringEscapeUtils.escapeHtml4((String)request.getAttribute(ERROR_ATTR_KEY));
	    String publicationType = (String)request.getAttribute("publicationType");
		String objId = (String)request.getAttribute("objId");
		String appId = (String)request.getAttribute("appId");

	    // HTML出力
	    out.println("<html>");
	    out.println("<head>");
	    out.println("	<title>ERROR</title>");
	    out.println("	<meta http-equiv=\"Content-Type\" content=\"text/html;charset=UTF-8\">");
	    out.println("	<meta http-equiv=\"Pragma\" content=\"no-cache\">");
	    out.println("	<meta http-equiv=\"cache-control\" content=\"no-cache\">");
	    out.println("	<meta http-equiv=\"Expires\" content=\"Thu,01 Dec 1994 16:00:00 GMT\">");
	    out.println("	<style type=\"text/css\">");
	    out.println("		<!--");
	    out.println("		body");
	    out.println("		{");
	    out.println("			margin-top:		0px;");
	    out.println("			margin-bottom:	0px;");
	    out.println("			margin-left:	0px;");
	    out.println("			margin-right:	0px;");
	    out.println("		}");
	    out.println("		-->");
	    out.println("	</style>");
	    out.println("	<script language=\"javascript\">");
	    out.println("	<!--");
	    out.println("		alert(\"" + message + "\");");

	    out.println("		var publicationType = \"" + publicationType + "\";");
	    out.println("		var appId = \"" + appId + "\";");
		out.println("		var url = null;");
		out.println("		if (appId && appId == \"html\") {");
		out.println("			url = \"../client#/documents/login\";");
		out.println("		} else {");
		out.println("			url = \"../app/document/index.jsp\";");
		out.println("		}");

		out.println("		if(publicationType == \"private\"){");

		out.println("			window.location.href = url + \"?objId=" + objId + "&privateFileDownloadObjId=" + objId + "\";");

		out.println("		}else{");
		out.println("			window.location.href = url + \"?objId=" + objId + "&publicFileDownloadObjId=" + objId + "\";");
		out.println("		}");

	    out.println("	//-->");
	    out.println("	</script>");
	    out.println("</head>");
	    out.println("");
	    out.println("<body>");
	    out.println("</body>");
	    out.println("</html>");

	}
}
