package servlet.dl;

import common.util.AppConstant;
import common.util.AppObjectConditionHelper;

import eim.bo.EIMAccessRole;
import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.FileUtils;
import eim.util.SecurityUtils;

/**
 * 公開ドキュメントをダウンロードするDownloadサーブレット。
 * <p>
 * このクラスは
 * <code>{@link AbstractDownloadDocument}</code>
 * を拡張している。
 * 
 * @version	1.0.0
 * @since		1.0.0
 */
public class DownloadPublicDocument extends AbstractDownloadDocument
{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * ダウンロード処理実行に必要なアクセス権限をチェックする。
	 * 
	 * @return int アクセス権限
	 */
	boolean checkAccessRight(EIMSession session, EIMObject object, AppObjectConditionHelper helper) throws Exception {
		// セキュリティ設定無の場合はfalse
		if ( object.getSecurityId() == AppConstant.NO_SECUTIRY ) {
			return false;
		}
		
		// 権限チェック
		// ステータス判定も含めてチェック
		boolean result = helper.checkAccessibleStatusSelf(object, false);
		return result;
	}
	
	/**
	 * ダウンロード対象のオブジェクトを取得する。
	 * 
	 * @return EIMObject オブジェクト
	 */
	EIMObject getAlternateObject(EIMSession session, EIMObject object, AppObjectConditionHelper helper) throws Exception {
		return object;
	}
	
	/**
	 * フォーマットの取得する。
	 * 
	 * @return EIMFormat フォーマット
	 */
	EIMFormat getFormat(EIMSession session, EIMObject object) throws Exception {
		EIMFormat format = FileUtils.getFormatByName(session, EIMConfig.get("FORMAT_NAME_PUBLIC"));
		return format;
	}
}