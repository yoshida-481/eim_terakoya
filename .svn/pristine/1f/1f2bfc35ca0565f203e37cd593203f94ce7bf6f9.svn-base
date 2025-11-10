package common.util;

import org.apache.commons.configuration2.ConfigurationUtils;
import org.apache.commons.configuration2.HierarchicalConfiguration;
import org.apache.commons.configuration2.builder.fluent.Configurations;

import eim.util.EIMConfig;

/**
 * 公開処理アドオン用ファイルクラス
 *
 *<li>初回のみ設定ファイルから取得して、以降はシングルトンで保持し続けます。
 */
public class PublishAddonXMLConfig {

	/**
	 * 当クラスのインスタンス
	 */
	private static PublishAddonXMLConfig xmlConf_ = null;
	
	/**
	 * 公開処理アドオンの設定です。
	 */
	private static HierarchicalConfiguration<?> publishAddonConfigXML = null;
	
	/**
	 * PublishAddonXMLConfigを取得します。
	 * 
	 * @return 当クラスのインスタンス
	 * @throws Exception 
	 */
	public static PublishAddonXMLConfig getInstance() throws Exception {
		
		if (xmlConf_ == null) {
			xmlConf_ = new PublishAddonXMLConfig();
		}
		return xmlConf_;
	}
	
	/**
	 * コンストラクタ
	 * 
	 * @throws Exception
	 */
	private PublishAddonXMLConfig() throws Exception {
		
		// 公開処理アドオン設定ファイル
		publishAddonConfigXML = ConfigurationUtils.convertToHierarchical(
				new Configurations().xml(EIMConfig.get("APP_PUBLISH_ADDON_CONF")));
		
	}

	/**
	 * 公開処理アドオンの設定を戻します。
	 * 
	 * @return 公開処理アドオンの設定
	 */
	public HierarchicalConfiguration<?> getPublishAddonConfig() {
		return publishAddonConfigXML;
	}	
}
