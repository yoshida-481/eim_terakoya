package common.util;

import org.apache.commons.lang3.StringUtils;
import org.w3c.dom.Document;

import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.integration.dao.util.XMLDocumentLoaderUtil;


/**
 * 
 * カスタマイズデフォルトテーブルクラス
 *
 */
public class CustomDefaultTableConfig {
	
	/**
	 * 当クラスのインスタンス
	 */
	private static CustomDefaultTableConfig _xmlConf = null;
	
	/** XMLファイル情報 */
	private Document doc = null;
	
	/** 属性タイプレイアウト設定ファイル定義情報コンフィグファイルデフォルトパス */
	private static final String CONF_FILE_DEFAULT_PATH = "/CustomDefaultTableConf.xml";
	
	/** 属性タイプレイアウト設定ファイルパス */
	private String filePath = null;
	
	/**
	 * 当クラスのインスタンスを取得します。
	 * 
	 * @return 当クラスのインスタンス
	 * @throws Exception 
	 */
	public static synchronized CustomDefaultTableConfig getInstance() throws Exception {
		if (_xmlConf == null) {
			_xmlConf = new CustomDefaultTableConfig();
		}
		return _xmlConf;
	}
	
	/**
	 * コンストラクタ
	 * 
	 * @throws Exception
	 */
	private CustomDefaultTableConfig() throws Exception {
		filePath = ConfigUtils.getByKey("CUSTOM_DEFAULT_TABLE_CONF");
		
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