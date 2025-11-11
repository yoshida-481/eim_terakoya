package jp.co.ctc_g.eim.app.document.common.aop.plugin.impl;

import jp.co.ctc_g.eim.framework2.business.domain.box.BoxFileCopyFromEIMFileDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.common.aop.plugin.ParameterConverterPlugIn;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;

/**
 * パラメータ変換プラグインの実装クラス
 * <br>
 * 入力値としてオブジェクトIDを受け取り、「オブジェクト名」を取得するプラグインクラスです。
 * <br>
 * @since Ver6.6
 */
public class ObjectIDToNameConverterPlugInImpl implements
		ParameterConverterPlugIn {

	/**
	 * パラメータ変換の処理。<br>
	 * <br>
	 * 引数で指定されたオブジェクトのオブジェクト名を取得して返却します。
	 * <br>
	 * @param source オブジェクトID
	 * @return オブジェクト名
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 * @since Ver6.6
	 */
	public Object convert(Object source) throws Exception {

		long id = -1;
		if (source instanceof BoxFileCopyFromEIMFileDomain) {
			id = ((BoxFileCopyFromEIMFileDomain) source).getObjectId();
		}

		ObjectService objectService = (ObjectService)ApplicationContextLoader.getApplicationContext().getBean("objectServiceWithoutAttribute");
		ObjectDomain document = objectService.getById(id);

		return document.getName();
	}

}
