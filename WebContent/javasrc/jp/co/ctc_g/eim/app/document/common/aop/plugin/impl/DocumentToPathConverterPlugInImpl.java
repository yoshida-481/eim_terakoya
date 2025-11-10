package jp.co.ctc_g.eim.app.document.common.aop.plugin.impl;

import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.common.aop.plugin.ParameterConverterPlugIn;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;

/**
 * パラメータ変換プラグインの実装クラス
 * <br>
 * 入力値としてObjectDomainを受け取り、属性として保持されている「パス」を取得するプラグインクラスです。
 * <br>
 * @since Ver6.6
 */
public class DocumentToPathConverterPlugInImpl implements
		ParameterConverterPlugIn {

	/**
	 * パラメータ変換の処理。<br>
	 * <br>
	 * 引数で指定されたドキュメントのパス属性を取得して返却します。
	 * <br>
	 * @param source ドキュメント情報
	 * @return パス属性の値
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 * @since Ver6.6
	 */
	public Object convert(Object source) throws Exception {
		ObjectDomain document = (ObjectDomain)source;
		String path = document.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS")).getString();
		
		return path;
	}

}
