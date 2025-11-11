package servlet.dl;

import java.util.Iterator;
import java.util.List;

import common.util.AppConstant;
import common.util.AppObjectConditionHelper;

import eim.bo.EIMAccessRole;
import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRelation;
import eim.bo.EIMRelationType;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.FileUtils;
import eim.util.ObjectUtils;
import eim.util.RelationUtils;
import eim.util.SecurityUtils;

/**
 * 非公開ドキュメントをダウンロードするDownloadサーブレット。
 * <p>
 * このクラスは
 * <code>{@link AbstractDownloadDocument}</code>
 * を拡張している。
 * 
 * @version	1.0.0
 * @since		1.0.0
 */
public class DownloadMyDocument extends AbstractDownloadDocument
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
		// マイドキュメントとのリレーションのチェック
		EIMUser user = session.getUser();
		
		EIMObjectType objTypeMyDoc = ObjectUtils.getObjectTypeByName(session,  EIMConfig.get("OBJECT_TYPE_NAME_MYDOCUMENT"));
		
		EIMRelationType relTypeDoc = RelationUtils.getRelationTypeByName(session, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		
		List relations = RelationUtils.getParentRelationListByType(session, object, relTypeDoc, objTypeMyDoc,EIMAccessRole.READ);
		
		EIMObject myDocObj = ObjectUtils.getObjectByTypeAndName(session, objTypeMyDoc, user.getCode());
		if(myDocObj == null || !SecurityUtils.authorized(session, myDocObj, user, EIMAccessRole.READ))
		{
			return false;
		}

		// ドキュメントの親オブジェクトに自分のマイドキュメントフォルダが含まれていればtrue
		for (Iterator i = relations.iterator(); i.hasNext();) {
			EIMRelation relation = (EIMRelation)i.next();
			EIMObject parentObject = relation.getParent();
			
			if (parentObject.getId() == myDocObj.getId()) {
				return true;
			}
		}
		
		return false;
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
		EIMFormat format = FileUtils.getDefaultFormat(session, object.getType());
		return format;
	}
}