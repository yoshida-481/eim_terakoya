package eim.command.business.service.execute;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

import common.util.AppConstant;
import common.util.AppObjectConditionHelper;
import common.util.AppUpdateNoticeUtils;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMRelation;
import eim.bo.EIMRelationType;
import eim.bo.EIMResource;
import eim.bo.EIMValueType;
import eim.command.business.service.EIMCommandService;
import eim.command.business.service.result.EIMCommandResult;
import eim.command.business.service.result.EIMCommandResultDocument;
import eim.command.common.util.EIMCommandConstant;
import eim.command.common.util.EIMCommandDisplayColorUtils;
import eim.command.common.util.EIMCommandUtil;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.DateUtils;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.OperationHistoryUtils;
import eim.util.RelationUtils;
import eim.util.SecurityUtils;
import eim.util.StringUtils;
import eim.util.TypeConvertUtils;

/**
 * updateAttrコマンド用コマンド実行実装クラス
 *
 */
public class EIMCommandUpdateAttrExecuter extends EIMCommandExecuter {

	// temporary field
	private List<String> objIds = new ArrayList<String>();
	private List<String> attNames = new ArrayList<String>();
	private List<String> attValues = new ArrayList<String>();

	private final int REAL_NUMBER_VALUE_LENGTH_OVER = 207;
	private final int STRING_VALUE_LENGTH_OVER = 208;
	private final int TEXT_VALUE_LENGTH_OVER = 209;

	private String objId;
	private EIMRelationType relTypeDoc;
	private HashMap<String, String> attributeMap = new HashMap<String, String>();

	public EIMCommandUpdateAttrExecuter() {
	}

	/**
	 * @return the objId
	 */
	public String getObjId() {
		return objId;
	}

	/**
	 * @param objId the objId to set
	 */
	public void setObjId(String objId) {
		this.objId = objId;
	}

	/**
	 * @return the attributes
	 */
	public HashMap<String, String> getAttributeMap() {
		return attributeMap;
	}

	/**
	 * @param attributes the attributes to set
	 */
	public void setAttributeMap(HashMap<String, String> attributeMap) {
		this.attributeMap = attributeMap;
	}

	/* (非 Javadoc)
	 * @see command.business.service.execute.EIMCommandExecuter#setOtherParameter(java.lang.String, java.lang.String)
	 */
	@Override
	public void setOtherParameter(String key, String value) {
		if(key.equals(EIMCommandConstant.OBJID)) {
			objIds.add(value);
		} else if(key.equals(EIMCommandConstant.ATTNAME)) {
			attNames.add(value);
		} else if(key.equals(EIMCommandConstant.ATTVALUE)) {
			attValues.add(value);
		}
		super.setOtherParameter(key, value);
	}

	public EIMCommandResult execute() throws Exception{

		EIMCommandResultDocument resultData = new EIMCommandResultDocument(getSess());
		boolean renameFlag = false;
		try{
			//パラメータチェック
			checkParameter(resultData);

			EIMObject targetObj = ObjectUtils.getObjectById(getSess(), Long.valueOf(objId));
			if (targetObj == null) {
				setErrorCodeMessage(resultData, "EIM.ERROR.CODE.OJBECT.NO.EXIST", "EIM.ERROR.LOGIC.OJBECT.NO.EXIST");
				throw new EIMException();
			}
			//種別を設定する
			resultData.setTarget(targetObj);

			if(targetObj.getSecurity() != null) {
				//指定したオブジェクトの参照権限があるかチェック
				//公開読取権限以下の権限の場合
				if(!SecurityUtils.authorized(getSess(), targetObj, getSess().getUser(), EIMAccessRole.READ)) {
					setErrorCodeMessage(resultData, "EIM.ERROR.CODE.NO.READ.AUTH","EIM.ERROR.LOGIC.FILE.NO.READ.AUTH");
					throw new EIMException();
				}
				//指定したオブジェクトの更新権限があるかチェック
				if(!SecurityUtils.authorized(getSess(), targetObj, getSess().getUser(), EIMAccessRole.UPDATE)) {
					setErrorCodeMessage(resultData, "EIM.ERROR.CODE.OBJECT.NO.UPDATE.AUTH","EIM.ERROR.LOGIC.OBJECT.NO.UPDATE.AUTH");
					throw new EIMException();
				}
			}

			List<String> warningAttNames = new ArrayList<String>();
			List<Long> inheritedAttList = getInheritedAttList(targetObj);
			String nameAllotmentAttrName = getNameAllotmentAttributeName(targetObj, getSess());
			List<String> systemAttList = Arrays.asList(AppConstant.SYSTEM_ATTRIBUTE_DEFNAME);
			List<String> attNamesKeeper = new ArrayList<String>();
			for (String attName : attNames) {
				//属性を指定する際に、同じ属性名を複数入力された時に二回同じ動作をしないようにする
				if (!attNamesKeeper.contains(attName)) {
					if (systemAttList.contains(attName)) {
						// 指定した属性がシステム属性
						setErrorCodeMessage(resultData,"EIM.ERROR.CODE.CANNOT.UPDATE.SYSTEM.ATTRIBUTE","EIM.ERROR.LOGIC.CANNOT.UPDATE.SYSTEM.ATTRIBUTE",attName);
						throw new EIMException();
					}
					EIMAttributeType attType = AttributeUtils.getAttributeTypeByName(getSess(), attName);
					if (attType == null) {
						// 指定した属性が存在しない
						setErrorCodeMessage(resultData,"EIM.ERROR.CODE.ATTRIBUTE.NO.EXIST","EIM.ERROR.LOGIC.ATTRIBUTE.NO.EXIST");
						throw new EIMException();
					}
					if (inheritedAttList.contains(attType.getId())) {
						// 指定した属性が下位引き継ぎ属性
						warningAttNames.add(attName);
						continue;
					}
					if (nameAllotmentAttrName != null && nameAllotmentAttrName.equals(attName)) {
						// 指定した属性が名称割当て属性
						// 属性値が空
						if (StringUtils.isBlank(attributeMap.get(attName))) {
							setErrorCodeMessage(resultData, "EIM.ERROR.CODE.NO.NAMEALLOTMENTATTR.VALUE", "EIM.ERROR.LOGIC.NO.NAMEALLOTMENTATTR.VALUE", attName);
							throw new EIMException();
						}
						// オブジェクト名称を更新する
						String attValue = attributeMap.get(attName);
						ObjectUtils.rename(getSess(), targetObj, attValue);
						renameFlag = true;
					}
					
					
					try {
						if (attType.isMultiple()) {
							// 複数値属性の場合
							String valuesStr = attributeMap.get(attType.getName());
							List<String> attValueList = new ArrayList<String>();
							
							// 区切り文字で分割して複数値を取得
							if (!StringUtils.isBlank(valuesStr)) {
								String[] tmpValues = valuesStr.split(EIMConfig.getValue("EIM.COMMAND.SERPARATER"));
								for (int i = 0; i < tmpValues.length; i++) {
									if (!StringUtils.isBlank(tmpValues[i])) {
										attValueList.add(tmpValues[i]);
									}
								}
							}
							updateMultipleValueAttribute(targetObj, attType, attValueList);
							// 更新対象が「下位への引継ぎ属性」の場合、子オブジェクトに再帰的に属性を設定
							if (isSuccessionAttr(attType, targetObj)){
								updateMultipleValueAttributeRecurrently(targetObj, attType, attValueList);
							}
						}
						else {
							// 単数値属性の場合
							updateSingleValueAttribute(targetObj, attType);
							// 更新対象が「下位への引継ぎ属性」の場合、子オブジェクトに再帰的に属性を設定
							if (isSuccessionAttr(attType, targetObj)){
								updateSingleValueAttributeRecurrently(targetObj, attType);
							}
						}
					} catch (EIMException e) {
						int errCode = e.getCode();
						if (errCode == REAL_NUMBER_VALUE_LENGTH_OVER ||
								errCode == STRING_VALUE_LENGTH_OVER ||
								errCode == TEXT_VALUE_LENGTH_OVER){
							// DBへの設定許容値オーバー
							resultData.setTypeCodeMessage(EIMResource.getMessageValue("EIM.RESULT.TYPE.ERROR"), EIMResource.getMessageValue("EIM.ERROR.CODE.OVERFLOW.ATTR.VALUE"),
									e.getMessage() + ":" + attName);
						}
						throw new EIMException();
					} catch (NumberFormatException e) {
						// 数値型属性の値の変換ができない
						setErrorCodeMessage(resultData,"EIM.ERROR.CODE.INVALID.NUMERIC.ATTR","EIM.ERROR.LOGIC.INVALID.NUMERIC.ATTR");
						throw new EIMException();
					} catch (ParseException e) {
						// 日付型属性の値が変換できない
						setErrorCodeMessage(resultData,"EIM.ERROR.CODE.INVALID.DATE.FORMAT","EIM.ERROR.LOGIC.INVALID.DATE.FORMAT",attName);
						throw new EIMException();
					}
					attNamesKeeper.add(attName);
				}
			}

			if (resultData.getType()==null || !resultData.getType().equals(EIMResource.getMessageValue("EIM.RESULT.TYPE.ERROR"))){

					// 警告事項があれば実行結果に設定（＝下位引継ぎ属性あり）
					if (warningAttNames.size() > 0)
						setWarningMessage(resultData, "EIM.WARN.LOGIC.CANNOT.UPDATE.INHERITED.ATTRIBUTE", warningAttNames);

					// オブジェクト情報を実行結果に設定
					if (resultData.getType() == null)
						resultData.setType(EIMResource.getMessageValue("EIM.RESULT.TYPE.INFO"));
					targetObj = ObjectUtils.getObjectById(getSess(), targetObj.getId());
					resultData.setTarget(targetObj);


					// 操作履歴
					createOperationHistory(EIMCommandConstant.UPDATE_ATTRIBUTE_EXCOMMAND, EIMCommandConstant.TARGET_TO_UPDATE, targetObj, resultData);

					// アクセス履歴
					this.createAccessHistory(targetObj, "EIM.ACCESS.TYPE.EXIF.ATTRIBUTEUPDATE");
			}
			
			if( renameFlag ) {
				// SearchFramework 検索FW更新通知 対象：対象のドキュメントorフォルダorタグorワークスペース
				AppUpdateNoticeUtils.updateNoticeInsertObject(getSess(), targetObj, 
						"SEARCHFW_UPDATE_ATTR_RENAME_FOLDER", "SEARCHFW_UPDATE_ATTR_DOCUMENT",
						"SEARCHFW_UPDATE_ATTR_TAG", "SEARCHFW_UPDATE_ATTR_RENAME_WORKSPACE");
				
				// SearchFramework 検索FW更新通知 対象：名称割当改名時のみ、子オブジェクトも通知
				AppUpdateNoticeUtils.updateNoticeInsertChildRecursive(getSess(), targetObj, 
						"SEARCHFW_UPDATE_ATTR_CHILD_FOLDER", "SEARCHFW_UPDATE_ATTR_CHILD_DOCUMENT",
						"SEARCHFW_UPDATE_ATTR_CHILD_TAG", "SEARCHFW_UPDATE_ATTR_CHILD_DOCUMENTLINK");
			}
			else {
				// SearchFramework 検索FW更新通知 対象：対象のドキュメントorフォルダorタグorワークスペース
				AppUpdateNoticeUtils.updateNoticeInsertObject(getSess(), targetObj, 
						"SEARCHFW_UPDATE_ATTR_FOLDER", "SEARCHFW_UPDATE_ATTR_DOCUMENT",
						"SEARCHFW_UPDATE_ATTR_TAG", "SEARCHFW_UPDATE_ATTR_WORKSPACE");
			}
			
		}catch(EIMException e){
			return resultData;
		}catch(NumberFormatException e){
			setErrorCodeMessage(resultData,"EIM.ERROR.CODE.OBJID.NOT.NUM","EIM.ERROR.LOGIC.OBJID.NOT.NUM",objIds.get(0));
			return resultData;
		}catch(Exception e){
			throw e;
		}
        return resultData;
    }

	/**
	 * 複数値属性を更新する
	 * @param targetObj
	 * @param attType
	 * @throws Exception
	 */
	private void updateMultipleValueAttribute(EIMObject targetObj, EIMAttributeType attType, List<String> attValueList) throws Exception {
		
		// 更新処理
		if (attValueList == null || attValueList.size() <= 0) {
			// 属性値が空の場合は属性削除
			ObjectAttributeUtils.deleteAttribute(getSess(), targetObj, attType);
		}
		else {
			switch (attType.getValueType().getId()) {
				case EIMValueType.INTEGER:
					long[] intValues = new long[attValueList.size()];
					for (int i = 0; i < attValueList.size() ; i++ ){
						intValues[i] = Integer.parseInt(attValueList.get(i));
					}
					ObjectAttributeUtils.setAttribute(getSess(), targetObj, attType, TypeConvertUtils.convertToBuildTypeArray(intValues));
					for (String attValue : attValueList) {
						EIMCommandDisplayColorUtils.updateDisplayColor(getSess(), null, targetObj, attType, attValue);
					}
					break;
				case EIMValueType.DOUBLE:
					double[] doubleValues = new double[attValueList.size()];
					for (int i = 0; i < attValueList.size() ; i++ ){
						doubleValues[i] = Double.parseDouble(attValueList.get(i));
					}
					ObjectAttributeUtils.setAttribute(getSess(), targetObj, attType, doubleValues);
					for (String attValue : attValueList) {
						EIMCommandDisplayColorUtils.updateDisplayColor(getSess(), null, targetObj, attType, attValue);
					}
					break;
				case EIMValueType.STRING:
				case EIMValueType.TEXT:
					ObjectAttributeUtils.setAttribute(getSess(), targetObj, attType, attValueList.toArray(new String[attValueList.size()]));
					for (String attValue : attValueList) {
						EIMCommandDisplayColorUtils.updateDisplayColor(getSess(), null, targetObj, attType, attValue);
					}
					break;
				case EIMValueType.DATE:
					SimpleDateFormat format = new SimpleDateFormat(EIMConfig.getValue("EIM.COMMAND.DATE.FORMAT"));
					Date[] dateValues = new Date[attValueList.size()];
					for (int i = 0; i < attValueList.size() ; i++ ){
						// タイムゾーンを調整
						// 受け取った日付はアプリと同じタイムゾーンとして、DBサーバのタイムゾーンを考慮する
						dateValues[i] = DateUtils.editExpirationDate(getSess(), format.parse(attValueList.get(i)));
					}
					ObjectAttributeUtils.setAttribute(getSess(), targetObj, attType, dateValues);
					for (Date dateValue : dateValues) {
						EIMCommandDisplayColorUtils.updateDisplayColor(getSess(), null, targetObj, attType, dateValue);
					}
					break;
			}
		}
	}
	/**
	 * (単数値)属性を更新する
	 * @param targetObj
	 * @param attType
	 * @param attValue
	 * @throws Exception
	 */
	private void updateSingleValueAttribute(EIMObject targetObj, EIMAttributeType attType) throws Exception {
		
		String attValue = attributeMap.get(attType.getName());
		if (StringUtils.isBlank(attValue)) {
			// 属性値が空の場合は属性削除
			ObjectAttributeUtils.deleteAttribute(getSess(), targetObj, attType);
		}
		else {
			// 属性値と表示色オブジェクトを更新（Util内部にて表示色オブジェクトに紐付くかどうかは判断してくれる）
			switch (attType.getValueType().getId()) {
			case EIMValueType.INTEGER:
				ObjectAttributeUtils.setAttribute(getSess(), targetObj, attType, Long.parseLong(attValue));
				EIMCommandDisplayColorUtils.updateDisplayColor(getSess(), null, targetObj, attType, attValue);
				break;
			case EIMValueType.DOUBLE:
				ObjectAttributeUtils.setAttribute(getSess(), targetObj, attType,Double.parseDouble(attValue));
				EIMCommandDisplayColorUtils.updateDisplayColor(getSess(), null, targetObj, attType, attValue);
				break;
			case EIMValueType.STRING:
			case EIMValueType.TEXT:
				ObjectAttributeUtils.setAttribute(getSess(), targetObj, attType,attValue);
				EIMCommandDisplayColorUtils.updateDisplayColor(getSess(), null, targetObj, attType, attValue);
				break;
			case EIMValueType.DATE:
				// ワークフローで使用するフォーマットに修正
				Date valueDate = new SimpleDateFormat(EIMConfig.getValue("EIM.COMMAND.DATE.FORMAT")).parse(attValue);
				ObjectAttributeUtils.setAttribute(getSess(),targetObj,attType,DateUtils.editExpirationDate(getSess(), valueDate));
				EIMCommandDisplayColorUtils.updateDisplayColor(getSess(), null, targetObj, attType, valueDate);
				break;
			}
		}
	}
	
	/**
	 * 再帰的に複数値属性の値・表示色を設定
	 * @param parentObj 属性は設定済みという前提
	 * @param successionAttr 子オブジェクトに継承していく属性
	 * @throws EIMException
	 */
	@SuppressWarnings("unchecked")
	private void updateMultipleValueAttributeRecurrently(EIMObject parentObj, EIMAttributeType attrType, List<String> attValueList) throws Exception {

		// 再帰的にフォルダ及びドキュメントへ属性値をセット
		List<EIMRelation>  childRelListDoc = RelationUtils.getChildRelationListByRelType(getSess(), parentObj, getRelTypeDoc());
		for (int i = 0; i < childRelListDoc.size(); i++)
		{
			EIMRelation relation = (EIMRelation)childRelListDoc.get(i);
			EIMObject childObj = relation.getChild();
			AppObjectConditionHelper helper = new AppObjectConditionHelper(getSess());
			if (helper.isTypeOfFolder(childObj.getType())) {
				
				// フォルダの下位引継ぎ属性に対象属性が含まれていなければ、引継ぎ処理中断
				if (!isSuccessionAttr(attrType, childObj))
					continue;
			}
			
			List<Long> inheritedAttList = getInheritedAttList(childObj);
			if (inheritedAttList != null && inheritedAttList.size() > 0 
					&& inheritedAttList.contains(attrType.getId())) {
				
				// 子オブジェクトの「上位からの引継ぎ属性」に指定の属性が含まれていれば更新
				updateMultipleValueAttribute(childObj, attrType, attValueList);
				updateMultipleValueAttributeRecurrently(childObj, attrType, attValueList);
			}
		}
	}

	/**
	 * 再帰的にオブジェクトに属性値・表示色を設定
	 * @param parentObj 属性は設定済みという前提
	 * @param successionAttr 子オブジェクトに継承していく属性
	 * @throws EIMException
	 */
	@SuppressWarnings("unchecked")
	private void updateSingleValueAttributeRecurrently(EIMObject parentObj, EIMAttributeType attrType) throws Exception {

		// 再帰的なフォルダ及びドキュメントへの属性値セット
		List<EIMRelation>  childRelListDoc = RelationUtils.getChildRelationListByRelType(getSess(), parentObj, getRelTypeDoc());
		for (int i = 0; i < childRelListDoc.size(); i++)
		{
			EIMRelation relation = (EIMRelation)childRelListDoc.get(i);

			EIMObject childObj = relation.getChild();
			
			AppObjectConditionHelper helper = new AppObjectConditionHelper(getSess());
			if (helper.isTypeOfFolder(childObj.getType())) {
				
				// フォルダの下位引継ぎ属性に対象属性が含まれていなければ、引継ぎ処理中断
				if (!isSuccessionAttr(attrType, childObj))
					continue;
			}

			List<Long> inheritedAttList = getInheritedAttList(childObj);
			if (inheritedAttList != null && inheritedAttList.size() > 0 
					&& inheritedAttList.contains(attrType.getId())) {
				
				// 子オブジェクトの「上位からの引継ぎ属性」に指定の属性が含まれていれば更新
				updateSingleValueAttribute(childObj, attrType);
				updateSingleValueAttributeRecurrently(childObj, attrType);
			}
		}
	}

	/**
	 * 操作履歴を登録
	 * @param operationTypeNo 操作種別（の番号）
	 * @param targetInfoNo 操作対象情報（の番号）
	 * @param targetObj 操作対象のEIMObject ※なければnull
	 * @throws Exception
	 */
	protected void createOperationHistory(String operationTypeNo, String targetInfoNo, EIMObject targetObj ,EIMCommandResultDocument result) throws Exception {

		OperationHistoryUtils.create(getSess(), EIMCommandConstant.COMMAND, operationTypeNo, targetInfoNo,
										targetObj!=null ? EIMConstant.OBJECT : null, targetObj, null, null, null,
										EIMCommandService.VERSION + ":" + result.getPath());
	}


	/**
	 * パラメータ解析
	 * @param resultData
	 * @throws EIMException, NumberFormatException
	 */
	private void checkParameter(EIMCommandResult resultData) throws EIMException {

		// objIdがあるか
		if (objIds.size() < 1) {
			setErrorCodeMessage(resultData, "EIM.ERROR.CODE.NO.ESSENTIAL.PARAMETER", "EIM.ERROR.LOGIC.NO.ESSENTIAL.PARAMETER", EIMCommandConstant.OBJID);
			throw new EIMException();
		}
		// objIdが複数指定されていないか
		if (objIds.size() > 1) {
			setErrorCodeMessage(resultData, "EIM.ERROR.CODE.OVERLAPPING.SINGLE.PARAMETER", "EIM.ERROR.LOGIC.OVERLAPPING.SINGLE.PARAMETER", EIMCommandConstant.OBJID);
			throw new EIMException();
		}
		// objId空文字でないか
		if (objIds.size() == 1 && objIds.get(0) == "") {
			setErrorCodeMessage(resultData, "EIM.ERROR.CODE.NO.ESSENTIAL.PARAMETER", "EIM.ERROR.LOGIC.NO.ESSENTIAL.PARAMETER", EIMCommandConstant.OBJID);
			throw new EIMException();
		}
		// objIdが半角英数字であるか
		if (!EIMCommandUtil.isOneByteNum(objIds.get(0))) {
			setErrorCodeMessage(resultData, "EIM.ERROR.CODE.OBJID.NOT.NUM", "EIM.ERROR.LOGIC.OBJID.NOT.NUM");
			throw new EIMException();
		}

		objId = objIds.get(0);

		// attNameとattValueの数が揃っているか
		if (attNames.size() != attValues.size() || attNames.contains("")) {
			setErrorCodeMessage(resultData, "EIM.ERROR.CODE.INVALID.ATTRNAME.ATTRVALUE", "EIM.ERROR.LOGIC.INVALID.ATTRNAME.ATTRVALUE");
			throw new EIMException();
		}
		
		// 指定属性名称と値のMap生成
		for (int i = 0; i < attNames.size(); i++) {
			attributeMap.put(attNames.get(i), attValues.get(i));
		}
	}

	/**
	 * 上位オブジェクトからの引継ぎ属性を取得
	 * @param object
	 * @return
	 * @throws EIMException
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	private List<Long> getInheritedAttList(EIMObject child) throws EIMException, Exception {

		// 対象Objectの上位引継ぎ属性を取得
		List<Long> inheritedAttIdList = new ArrayList<Long>();
		EIMAttribute inheritedAttribute = child.getAttribute(EIMConfig.getValue("ATTR_NAME_DOCUMENT_FROM_HIGH_ATTR"));
		if (inheritedAttribute != null) {
			long[] inheritedAttIds = TypeConvertUtils.convertToLongArray(inheritedAttribute.getInts());
			for (long inheritedAttId : inheritedAttIds) {
				inheritedAttIdList.add(inheritedAttId);
			}
		}
		return inheritedAttIdList;
	}

	/**
	 * 対象属性タイプが下位への引継ぎを行うかをチェック
	 * @param object
	 * @return
	 * @throws EIMException
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	private boolean isSuccessionAttr(EIMAttributeType targetAttrType, EIMObject targetObj) throws Exception {

		boolean result = false;

		EIMAttribute successionAttr = targetObj.getAttribute(EIMConfig.getValue("ATTR_NAME_WORKSPACE_TO_LOW_ATTR"));
		
		if (successionAttr != null) {
			long[] successionAttrs = TypeConvertUtils.convertToLongArray(successionAttr.getInts());
	
			for (int i = 0; i < successionAttrs.length; i++){
				if (targetAttrType.getId() == successionAttrs[i]){
					result = true;
					break;
				}
			}
		}

		return result;
	}

	/**
	 * 名称割当て属性の属性名称取得
	 * @param object
	 * @return
	 * @throws EIMException
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	private String getNameAllotmentAttributeName(EIMObject obj, EIMSession sess) throws EIMException, Exception {

		String nameAllotmentAttributeName = null;
		if (obj != null) {
			EIMAttributeType nameAllotmentAttributeType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.getValue("ATTR_NAME_FOLDER_NAME_ATTR"));
			EIMAttribute attr = obj.getAttribute(nameAllotmentAttributeType.getDefaultName());
			if (attr != null) {
				nameAllotmentAttributeName = AttributeUtils.getAttributeTypeById(sess, attr.getInt()).getDefaultName();
			}
		}
		return nameAllotmentAttributeName;
	}

	/**
	 * エラー情報を設定
	 * @param resultData
	 * @param codeKey
	 * @param messageKey
	 * @throws EIMException
	 */
	public void setErrorCodeMessage(EIMCommandResult resultData, String codeKey, String messageKey) throws EIMException{
		resultData.setTypeCodeMessage(EIMResource.getMessageValue("EIM.RESULT.TYPE.ERROR"), EIMResource.getMessageValue(codeKey), EIMResource.getMessageValue(messageKey));
	}
	/**
	 * エラー情報を設定
	 * @param resultData
	 * @param codeKey
	 * @param messageKey
	 * @param replacement メッセージ中の{0}を指定の文字列に置換する
	 * @throws EIMException
	 */
	public void setErrorCodeMessage(EIMCommandResult resultData, String codeKey, String messageKey, String replacement) throws EIMException{
		resultData.setTypeCodeMessage(EIMResource.getMessageValue("EIM.RESULT.TYPE.ERROR"), EIMResource.getMessageValue(codeKey), (EIMResource.getMessageValue(messageKey)).replace("{0}", replacement));
	}
	/**
	 * エラー情報を設定
	 * @param resultData
	 * @param codeKey
	 * @param messageKey
	 * @param replacement1 メッセージ中の{0}を指定の文字列に置換する
	 * @param replacement2 メッセージ中の{1}を指定の文字列に置換する
	 * @throws EIMException
	 */
	public void setErrorCodeMessage(EIMCommandResult resultData, String codeKey, String messageKey, String replacement1, String replacement2) throws EIMException{
		resultData.setTypeCodeMessage(EIMResource.getMessageValue("EIM.RESULT.TYPE.ERROR"), EIMResource.getMessageValue(codeKey), (EIMResource.getMessageValue(messageKey)).replace("{0}", replacement1).replace("{1}", replacement2));
	}
	/**
	 * 警告情報を設定
	 * @param resultData
	 * @param codeKey
	 * @param messageKey
	 * @throws EIMException
	 */
	public void setWarningMessage(EIMCommandResult resultData, String messageKey, List<String> replacements) throws EIMException{
		StringBuilder sb = new StringBuilder();
		String baseMessage = EIMResource.getMessageValue(messageKey);
		for (String replacement : replacements) {
			sb.append(baseMessage.replace("{0}", replacement));
		}
		resultData.setType(EIMResource.getMessageValue("EIM.RESULT.TYPE.WARN"));
		resultData.setMessage(sb.toString());
	}

	private EIMRelationType getRelTypeDoc() throws Exception{
		if (this.relTypeDoc == null){
			this.relTypeDoc = RelationUtils.getRelationTypeByName(getSess(), EIMConfig.getValue("RELATION_TYPE_NAME_DOCUMENT"));
		}
		return relTypeDoc;
	}
}
