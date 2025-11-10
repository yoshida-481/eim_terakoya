package eim.command.common.util;

import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import eim.command.business.service.result.EIMCommandResult;

public class EIMCommandUtil {
	/**
	 * エンコーダー
	 * @param param
	 * @return
	 * @throws Exception
	 */
	public static String encode(String param) throws Exception {
		return URLEncoder.encode(param, "UTF-8");
	}
	
	/**
	 * デコーダー
	 * @param param
	 * @return
	 * @throws Exception
	 */
	public static String decode(String param) throws Exception {
		return URLDecoder.decode(param, "UTF-8");
	}
	
	/**
	 * DOMドキュメントの取得
	 * @return
	 * @throws Exception
	 */
	public static Document getDocument() throws Exception{
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		DocumentBuilder db = dbf.newDocumentBuilder();
		Document doc = db.newDocument();
		return doc;
	}
	
	/**
	 * 要素type、code、messageを作成
	 * @param resultData
	 * @param root
	 * @param doc
	 * @return
	 * @throws Exception
	 */
	public static Document makeResponseElementTypeCodeMessage(EIMCommandResult resultData, Element root, Document doc) throws Exception{
		
		//Type
		Element type = doc.createElement("type");
		type.appendChild(doc.createTextNode(resultData.getType()));
		root.appendChild(type);
		
		//Code
		Element code = doc.createElement("code");
		code.appendChild(doc.createTextNode(resultData.getCode()));
		root.appendChild(code);
		
		//Message
		Element message = doc.createElement("message");
		message.appendChild(doc.createTextNode(resultData.getMessage()));
		root.appendChild(message);
	
		return doc;
	}
	
	
	/** 
     * 文字列が半角数字であるかチェックする
     * true：半角数字である場合、false：半角数字でない場合
     * @param param
     */
	public static boolean isOneByteNum(String param) 
	{ 
		Pattern p = Pattern.compile("[0-9]+");
		Matcher m = p.matcher(param); 
		return m.matches(); 
	} 

}
