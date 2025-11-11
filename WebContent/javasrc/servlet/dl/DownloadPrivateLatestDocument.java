package servlet.dl;

import common.util.AppConstant;
import common.util.AppObjectConditionHelper;

import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.net.EIMSession;
import eim.util.FileUtils;
import eim.util.SecurityUtils;

/**
 * 承認済み最新版の公開ドキュメントをダウンロードするDownloadサーブレット。
 * <p>
 * このクラスは
 * <code>{@link DownloadPublicDocument}</code>
 * を拡張している。
 */
public class DownloadPrivateLatestDocument extends AbstractDownloadLatestDocument
{
	
	/**
	 * シリアルID
	 */
	private static final long serialVersionUID = 1L;
	
	/**
	 * ダウンロード処理実行に必要なアクセス権限をチェックする。
	 * 
	 * @param session EIMセッション
	 * @param object EIMオブジェクト
	 * @return boolean 権限あり:true、権限なし:false
	 */
	@Override
	boolean checkAccessRight(EIMSession session, EIMObject object, AppObjectConditionHelper helper) throws Exception {
		
		if(!SecurityUtils.authorized(session, object, session.getUser(), AppConstant.ACCESS_ROLE_ALWAYS_READ)){
			// 常時読取権限がない場合、権限なし
			return false;
		}
		
		return true;
	}
	
	/**
	 * フォーマットの取得する。
	 * 
	 * @param session EIMセッション
	 * @param object EIMオブジェクト
	 * @return EIMFormat フォーマット
	 */
	@Override
	EIMFormat getFormat(EIMSession session, EIMObject object) throws Exception {
		EIMFormat format = FileUtils.getDefaultFormat(session, object.getType());
		return format;
	}
	@Override
	String getPublicationType() throws Exception{
		return "private";
	}
}