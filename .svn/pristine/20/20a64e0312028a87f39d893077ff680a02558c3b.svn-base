package common.util;

import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMWorkFlow;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.StringUtils;
import eim.util.WorkFlowUtils;

/**
 *
 * OCR関連クラス
 *
 */
public class AppOcrUtil {
	
	/**
	 * OCR変換対象かどうかを判定
	 * 
	 * @param sess EIMセッション
	 * @param object EIMオブジェクト
	 */
	public static boolean isOcrTarget(EIMSession sess, EIMObject object) throws Exception
	{
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		
		// ドキュメントかどうかを判定
		if(!helper.isTypeOfDocument(object.getType())){
			return false;
		}
		
		// 拡張子の判定
		String fileExt = StringUtils.nullToBlank(StringUtils.getFileExt(object.getName()));
		if(!fileExt.equalsIgnoreCase(".pdf")){
			// PDF以外の場合は不可
			return false;
		}
		
		return true;
	}
	
	/**
	 * OCR設定有無があるかを判定する
	 * 
	 * @param sess EIMセッション
	 * @param object EIMオブジェクト
	 */
	public static boolean hasOcrSetting(EIMSession sess, EIMObject object) throws Exception
	{
		// ワークフロー設定オブジェクトを取得
		EIMWorkFlow workflow = WorkFlowUtils.getWorkFlowByType(sess, object.getType());
		if(workflow != null){
			EIMObject wfSettingObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"), "" + workflow.getId());
			EIMAttribute defOcrAttribute = wfSettingObj.getAttribute(EIMConfig.get("ATTR_NAME_OCR_SETTING_EXISTENCE"));
			long defOcr = 0;
			if(defOcrAttribute != null){
				defOcr = defOcrAttribute.getInt();
			}
			
			// 1:OCR設定有り、かつPDFの場合
			if(defOcr == AppConstant.OCR_SETTING_THERE && isOcrTarget(sess, object)){
				return true;
			}
			
		}
		
		return false;
	}
	
	/**
	 * OCR処理ステータスの値を設定する
	 * 
	 * @param sess EIMセッション
	 * @param object EIMオブジェクト
	 * @param status ステータス
	 */
	public static void setOcrProcessingStatus(EIMSession sess, EIMObject object, int status) throws Exception
	{
		
		EIMAttributeType attributeType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_OCR_PROCESS_STATUS"));
		
		if(status == AppConstant.OCR_PROC_STATUS_NONE){
			ObjectAttributeUtils.deleteAttribute(sess, object, attributeType);
		}else{
			ObjectAttributeUtils.setAttribute(sess, object, attributeType, status);
		}
		
	}
	
	/**
	 * OCR処理ステータスの値を取得する
	 * 
	 * @param sess EIMセッション
	 * @param object EIMオブジェクト
	 */
	public static long getOcrProcessingStatus(EIMSession sess, EIMObject object) throws Exception
	{
		long status = 0;
		
		EIMAttribute attribute = object.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_OCR_PROCESS_STATUS"));
		
		if(attribute == null){
			status = AppConstant.OCR_PROC_STATUS_NONE;
		}else{
			status = attribute.getInt();
		}
		
		return status;
	}
	
	/**
	 * OCR結果ステータスの値を取得する
	 * 
	 * @param sess EIMセッション
	 * @param object EIMオブジェクト
	 */
	public static long getOcrResultStatus(EIMSession sess, EIMObject object) throws Exception
	{
		long status = 0;
		
		EIMAttribute attribute = object.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_OCR_RESULT_STATUS"));
		
		if(attribute == null){
			status = AppConstant.OCR_RESULT_STATUS_NONE;
		}else{
			status = attribute.getInt();
		}
		
		return status;
	}
	
	/**
	 * OCR結果ステータスの値を設定す
	 * 
	 * @param sess EIMセッション
	 * @param object EIMオブジェクト
	 * @param status ステータス
	 */
	public static void setOcrResultStatus(EIMSession sess, EIMObject object, int status) throws Exception
	{
		
		EIMAttributeType attributeType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_OCR_RESULT_STATUS"));
		
		if(status == AppConstant.OCR_RESULT_STATUS_NONE){
			ObjectAttributeUtils.deleteAttribute(sess, object, attributeType);
		}else{
			ObjectAttributeUtils.setAttribute(sess, object, attributeType, status);
		}
		
	}
	
	/**
	 * OCR処理オブジェクトを作成する
	 * 
	 * @param sess EIMセッション
	 * @param object EIMオブジェクト
	 */
	public static void createOcrProcessingObject(EIMSession sess, EIMObject object) throws Exception
	{
		
		// OCR処理オブジェクトを取得する
		EIMObjectType ocrProcessingObjectType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_OCR_PROCESS"));
		EIMObject ocrProcessingObject = ObjectUtils.getObjectByTypeAndName(sess, ocrProcessingObjectType, String.valueOf(object.getId()));
		
		if(ocrProcessingObject == null){
			ObjectUtils.createObject(sess, ocrProcessingObjectType, String.valueOf(object.getId()));
		}
		
	}
	
	/**
	 * OCR処理オブジェクトを削除する
	 * 
	 * @param sess EIMセッション
	 * @param object EIMオブジェクト
	 */
	public static void deleteOcrProcessingObject(EIMSession sess, EIMObject object) throws Exception
	{
		
		// OCR処理オブジェクトを取得する
		EIMObjectType ocrProcessingObjectType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_OCR_PROCESS"));
		EIMObject ocrProcessingObject = ObjectUtils.getObjectByTypeAndName(sess, ocrProcessingObjectType, String.valueOf(object.getId()));
		
		if(ocrProcessingObject != null){
			// OCR処理オブジェクトを削除する
			ObjectUtils.deleteObject(sess, ocrProcessingObject);
		}
		
	}
	
	/**
	 * OCR処理に関連する属性・オブジェクトを削除する
	 * 
	 * @param sess EIMセッション
	 * @param object EIMオブジェクト
	 */
	public static void deleteAllRelatedToOcr(EIMSession sess, EIMObject object) throws Exception
	{
		setOcrProcessingStatus(sess, object, AppConstant.OCR_PROC_STATUS_NONE);
		setOcrResultStatus(sess, object, AppConstant.OCR_RESULT_STATUS_NONE);
		deleteOcrProcessingObject(sess, object);
	}
	
	/**
	 * OCR処理ステータスとOCR結果ステータスをコピーする。
	 * 
	 * @param sess EIMセッション
	 * @param fromObject コピー元EIMオブジェクト
	 * @param toObject コピー先EIMオブジェクト
	 */
	public static void copyOcrStatus(EIMSession sess, EIMObject fromObject, EIMObject toObject) throws Exception
	{
		
		EIMWorkFlow workflow = WorkFlowUtils.getWorkFlowByType(sess, fromObject.getType());
		
		if(workflow != null){
			// WFありの場合
			
			// OCR処理ステータスが未設定以外の場合
			if(getOcrProcessingStatus(sess, fromObject) != AppConstant.OCR_PROC_STATUS_NONE){
				// OCR処理ステータスを処理待ちに設定
				setOcrProcessingStatus(sess, toObject, AppConstant.OCR_PROC_STATUS_PROCESS_WAIT);
			}else{
				// OCR処理ステータスを未設定に設定
				setOcrProcessingStatus(sess, toObject, AppConstant.OCR_PROC_STATUS_NONE);
			}
			
			// OCR結果ステータスを削除
			setOcrResultStatus(sess, toObject, AppConstant.OCR_RESULT_STATUS_NONE);
			
		}else{
			// WFなしの場合
			
			// OCR処理ステータスが処理完了かつ、OCR結果ステータスが成功の場合
			if(getOcrProcessingStatus(sess, fromObject) == AppConstant.OCR_PROC_STATUS_PROCESS_COMPLETE
					&& getOcrResultStatus(sess, fromObject) == AppConstant.OCR_RESULT_STATUS_SUCCESS)
			{
				// OCR処理ステータスを処理完了に設定
				setOcrProcessingStatus(sess, toObject, AppConstant.OCR_PROC_STATUS_PROCESS_COMPLETE);
				// OCR結果ステータスを成功に設定
				setOcrResultStatus(sess, toObject, AppConstant.OCR_RESULT_STATUS_SUCCESS);
			}else{
				// OCR処理ステータスとOCR結果ステータスを削除
				deleteAllRelatedToOcr(sess, toObject);
			}
		}
		
	}
	
	/**
	 * OCR処理設定可否チェック
	 * 
	 * @param object EIMオブジェクト
	 */
	public static boolean isSettable(EIMObject object){
		
		if(object.getStatus() == null){
			// ステータスがない場合、設定不可能
			return false;
		}
		
		if(object.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_NONE
		&& object.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC){
			// ステータスが「なし」「公開済」以外の場合、設定可能
			return true;
			
		}else{
			// ステータスが「なし」「公開済」の場合、設定不可能
			return false;
			
		}
	}
	
}