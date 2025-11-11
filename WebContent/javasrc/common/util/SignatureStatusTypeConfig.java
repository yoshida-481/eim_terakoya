package common.util;

import org.apache.commons.lang3.StringUtils;
import org.w3c.dom.Document;
import org.xml.sax.SAXParseException;

import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.integration.dao.util.XMLDocumentLoaderUtil;


/**
 * 
 * PDF署名バッチ(IOWebDoc)用 クラス
 * 定義情報XMLファイルの取得
 */
public class SignatureStatusTypeConfig {
	
	/**
	 * 当クラスのインスタンス
	 */
	private static SignatureStatusTypeConfig _xmlConf = null;
	
	/** XMLファイル情報 */
	private Document doc = null;
	
	/** PDF署名ステータスタイプ欄設定ファイル デフォルトパス */
	private static final String CONF_FILE_DEFAULT_PATH = "/SignatureStatusTypeConf.xml";
	
	/** PDF署名ステータスタイプ欄設定ファイルパス */
	private String filePath = null;
	
	/**
	 * 当クラスのインスタンスを取得します。
	 * 
	 * @return 当クラスのインスタンス
	 * @throws Exception 
	 */
	public static synchronized SignatureStatusTypeConfig getInstance() throws Exception,SAXParseException {
		if (_xmlConf == null) {
			_xmlConf = new SignatureStatusTypeConfig();
		}
		return _xmlConf;
	}
	
	/**
	 * コンストラクタ
	 * 
	 * @throws Exception
	 */
	private SignatureStatusTypeConfig() throws Exception,SAXParseException {
		filePath = ConfigUtils.getByKey("SIGNATURE_STATUS_TYPE_CONF");
		
		// 定義情報コンフィグファイルパスがNULL又は空白の場合、デフォルトパスを設定
		if(StringUtils.isBlank(filePath)){
			filePath = CONF_FILE_DEFAULT_PATH;
		}
		
		doc = XMLDocumentLoaderUtil.getDocument(filePath);
	}
	
	/**
	 * XMLファイル情報を取得する
	 * 
	 * @return XMLファイル情報
	 */
	public Document getDocument() {
		return doc;
	}
	
	/**
	 * XMLファイルパスを取得する
	 * 
	 * @return XMLファイル情報
	 */
	public String getFilePath() {
		return filePath;
	}
	
}