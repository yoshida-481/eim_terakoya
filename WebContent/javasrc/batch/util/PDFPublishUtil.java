package batch.util;

import java.util.ArrayList;
import java.util.List;

import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import app.document.approve.ApproveCommonUtil;

import common.util.AppMessageUtils;
import common.util.AppObjectUtil;
import common.util.MailUtil;

import eim.bo.EIMAccessRole;
import eim.bo.EIMAttributeType;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRelation;
import eim.bo.EIMResource;
import eim.bo.EIMVersion;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.RelationUtils;
import eim.util.SecurityUtils;
import eim.util.VersionUtils;
import eim.util.WorkFlowUtils;

/**
 * 
 * PDFバッチ処理用共通処理クラス
 *
 */
public class PDFPublishUtil {
	/** Error Logging */
	private static Log log = LogFactory.getLog(PDFPublishUtil.class);
	
	/**
	 *公開処理用ステータス更新
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param object ステータス更新対象オブジェクト
	 * @throws Exception
	 */
	public static void pdfStatusUpCommon(EIMSession sess, EIMObject object) throws Exception
	{
		
		// ステータス更新
		try
		{
			object = WorkFlowUtils.statusUp(sess, object);
		}
		catch(Exception e)
		{
			log.warn(" " + EIMResource.getMessage("EIM.ERROR.LOGIC.FAILEDSTATUSUP"));
			log.warn(AppMessageUtils.makeLogMessage(e.getMessage()), e);
			throw e;
		}
		AppObjectUtil.updateStatusRecurrently(sess, object, object.getStatus());
		
		List chldObjectList = AppObjectUtil.getChildEIMObjectRecurrently(sess, 
				object, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		chldObjectList.add(object);

		for (int j = 0; j < chldObjectList.size(); j++)
		{
			EIMObject chldObject = (EIMObject)chldObjectList.get(j);

			if(chldObject.getRev() > 0)
			{
				//Set Latest
				chldObject = VersionUtils.setLatest(sess, chldObject);
				
				//Old Revision
				EIMVersion version = VersionUtils.getVersion(sess, chldObject);
				EIMObject latestObj = version.getObjectByRev(chldObject.getRev() - 1);
				
				//Unlock (これを実行しないとステータスが「改訂中」のまま残る)
				ObjectUtils.unLock(sess, latestObj);
				
				//Parent Relation
				List parentRelList = RelationUtils.getParentRelationListByRelType(sess, latestObj, RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT")));
				
				try
				{
					//Delete Relation
					for(int k = 0; k < parentRelList.size(); k++)
					{
						EIMRelation relation = (EIMRelation)parentRelList.get(k);
						RelationUtils.deleteRelation(sess, relation);
					}
				}
				catch(Exception e)
				{
					log.warn(" " + EIMResource.getMessage("EIM.ERROR.LOGIC.FAILEDDELREL"));
					log.warn(AppMessageUtils.makeLogMessage(e.getMessage()), e);
					throw e;
				}
			}
		}
	}

	/**
	 *公開処理用公開通知
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param object 公開通知対象オブジェクト
	 * @throws Exception
	 */
	public static void pdfPubishNotifyCommon(EIMSession sess, EIMObject object) throws Exception
	{
		//Object Type Publish Mail
		EIMObjectType objTypePublishMail = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_PUBLICMAIL"));
		
		//Object Type NotifyTo
		EIMObjectType objTypeNotifyTo = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_NOTIFY_TO"));
		
		//公開通知オブジェクト
		EIMObject publishMailObj = ObjectUtils.getObjectByTypeAndName(sess, objTypePublishMail, String.valueOf(object.getId()));
		List pblisherList = new ArrayList();
		if(publishMailObj != null)
		{
			List notifyList = ObjectUtils.getObjectListByTypeAndName(sess, objTypeNotifyTo, String.valueOf(publishMailObj.getId()));
			for(int j = 0 ; j < notifyList.size() ; j++)
			{
				pblisherList.addAll(ApproveCommonUtil.getUserFromCode(sess, ((EIMObject)notifyList.get(j)).getAttribute("通知先種別：ID").getString()));
			}
		}
		
		//****************************
		// Send Publish Mail（即送信）
		//****************************
		if(publishMailObj != null)
		{
			if(publishMailObj.getAttribute("通知タイミング").getInt() == 0)
			{
				try
				{
					// 公開通知メールを送信
//					MailUtil.sendPublishedMail(sess, pblisherList, object);
					MailUtil.execute(sess, object, "MailPublicBatch");

					//Delete Publish Mail Object
					ObjectUtils.deleteObject(sess, publishMailObj);
				}
				catch(Exception e)
				{
					log.warn(" " + EIMResource.getMessage("EIM.ERROR.LOGIC.FAILEDSENDMAIL"));
					log.warn(AppMessageUtils.makeLogMessage(e.getMessage()), e);
					throw e;
				}
			}
			//****************************
			// Send Publish Mail（定時送信）
			//****************************
			else
			{
				// 公開通知オブジェクトのタイミングを"1"（定時）にすることが、
				// 定時送信メールで送信されるためのトリガとなる。
				// （ここに入るケースは"2"（定時(未公開)）になっているはず）
				EIMAttributeType attType = null;
				attType = AttributeUtils.getAttributeTypeByName(sess, "通知タイミング");
				ObjectAttributeUtils.setAttribute(sess, publishMailObj, attType, 1);
			}
		}
	}
}	
