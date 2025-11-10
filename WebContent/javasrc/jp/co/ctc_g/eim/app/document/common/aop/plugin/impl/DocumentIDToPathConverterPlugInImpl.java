package jp.co.ctc_g.eim.app.document.common.aop.plugin.impl;

import jp.co.ctc_g.eim.framework2.business.domain.box.BoxFileCopyFromEIMFileDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.common.aop.plugin.ParameterConverterPlugIn;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;

/**
 * パラメータ変換プラグインの実装クラス
 * <br>
 * 入力値としてドキュメントIDを受け取り、属性として保持されている「パス」を取得するプラグインクラスです。
 * <br>
 * @since Ver6.6
 */
public class DocumentIDToPathConverterPlugInImpl implements
		ParameterConverterPlugIn {

	/**
	 * パラメータ変換の処理。<br>
	 * <br>
	 * 引数で指定されたドキュメントのパス属性を取得して返却します。
	 * <br>
	 * @param source ドキュメントID
	 * @return パス属性の値
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 * @since Ver6.6
	 */
	public Object convert(Object source) throws Exception {

		long id = -1;
		if (source instanceof BoxFileCopyFromEIMFileDomain) {
			id = ((BoxFileCopyFromEIMFileDomain) source).getObjectId();
		} else if (source instanceof ObjectDomain) {
			id = ((ObjectDomain) source).getId();
		}

		ObjectService objectService = (ObjectService)ApplicationContextLoader.getApplicationContext().getBean("objectService2");
		ObjectDomain document = objectService.getById(id);

		String path = document.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS")).getString();

		return path;
	}

}
