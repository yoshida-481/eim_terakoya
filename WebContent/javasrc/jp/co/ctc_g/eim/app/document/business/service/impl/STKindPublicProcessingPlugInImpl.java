package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.io.File;

import eim.bo.EIMFile;
import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.FileUtils;
import jp.co.ctc_g.eim.framework.business.domain.StatusTypeKindExecDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.StatusTypeKindPlugInImpl;
import jp.co.ctc_g.eim.framework.common.exception.EIMSysException;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * ステータスタイプ種別「公開処理中」
 *
 */
public class STKindPublicProcessingPlugInImpl extends StatusTypeKindPlugInImpl {
	/**
	 * PDF変換用に原本ファイルをBoxから復帰させます。
	 * @see jp.co.ctc_g.eim.framework.business.service.StatusTypeKindPlugIn#entry(jp.co.ctc_g.eim.framework.business.domain.StatusTypeKindExecDomain)
	 */
	public void entry(StatusTypeKindExecDomain statusTypeKindExecDomain)
			throws Exception {

		if (!Boolean.valueOf(EIMConfig.get("USE_BOX_INTEGRATION_OPTION"))) {
			return;
		}

		File orgFile = null;
		try {
			EIMSession sess = EIMThreadContext.getEIMSession();
			ObjectDomain objectDomain = statusTypeKindExecDomain.getObject();

			// format 取得
			EIMObjectType objectType = new EIMObjectType(objectDomain.getObjectType().getId(), null, null);
			EIMFormat format = FileUtils.getDefaultFormat(sess, objectType);
			if (format == null) {
				// フォルダの場合は処理不要
				return;
			}

			// file 取得
			EIMObject object = new EIMObject(objectDomain.getId(),
					null,
					null,
					0,
					false,
					null,
		            null,
		            null,
		            null,
		            null,
		            null,
		            false,
		            false,
		            null
					);
			EIMFile file = FileUtils.getFile(sess, object, format);
			if (file == null) {
				// フォルダの場合は処理不要
				return;
			}

			// Boxからのファイル復帰
			FileUtils.prepareFileAccess(sess, object, file);
			orgFile = new File(file.getDirectory().getPath() + FileUtils.getFileName(object, file)); // シーケンスは考慮不要
		} catch (Exception e) {
			EIMSysException ese = new EIMSysException("EIM.ERROR.LOGIC.BOX.FILE.EXTRACT.ERROR", new Object[]{orgFile != null ? orgFile.getPath() : ""});
			ese.initCause(e);
			throw ese;
		}

		if (orgFile == null || !orgFile.exists()) {
			throw new EIMSysException("EIM.ERROR.LOGIC.BOX.FILE.EXTRACT.ERROR", new Object[]{orgFile != null ? orgFile.getPath() : ""});
		}
	}


}
