package eim.command.business.service.response.impl;

import java.io.PrintWriter;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;

import eim.command.business.service.response.EIMCommandResponseMaker;
import eim.command.business.service.result.EIMCommandResult;

/**
 * DOMによるレスポンス返却抽象テンプレートクラス
 * 
 *
 */
public abstract class EIMCommandResponseMakerDOM implements EIMCommandResponseMaker{
	
	public void makeResponse(EIMCommandResult resultData, HttpServletRequest request, HttpServletResponse response) throws Exception{
		
		// レスポンスヘッダー設定
		this.setResponseHeader(response);
		
		// レスポンスの中身をDOMで作成
		Document doc = makeResponseContentsByDOM(resultData);
		
		// レスポンスに出力
		outputContentsToResponse(doc, response);
	}
	
	/**
	 * レスポンスヘッダに値を設定
	 * @param response
	 * @throws Exception
	 */
	private void setResponseHeader (HttpServletResponse response) throws Exception{
		//Set Content Type
		response.setContentType("text/xml; charset=utf-8");
		response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	}
	
	/**
	 * DOMでレスポンスの内容を作成。
	 * @param resultData
	 * @throws Exception
	 */
	abstract protected Document makeResponseContentsByDOM(EIMCommandResult resultData) throws Exception;


	/**
	 * DOMで作成した内容をレスポンスに出力
	 * @param doc
	 * @param response
	 * @throws Exception
	 */
	private void outputContentsToResponse(Document doc, HttpServletResponse response) throws Exception{

		PrintWriter pw = response.getWriter();

		TransformerFactory tff = TransformerFactory.newInstance();
		Transformer transformer = tff.newTransformer(); 
		
		transformer.transform(new DOMSource(doc), new StreamResult(pw));
		
		//Flush
		pw.flush();
		
		//Close
		pw.close();
	}

}
