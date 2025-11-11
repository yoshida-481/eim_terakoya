package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import common.bo.AttributeValueMaster;
import common.util.AppConstant;
import common.util.AppUpdateNoticeUtils;
import common.util.AttributeMasterUtil;
import common.util.DisplayColorUtil;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAttributeType;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMResource;
import eim.bo.EIMValueType;
import eim.net.EIMSession;
import eim.util.DateUtils;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;
import eim.util.StringUtils;
import jp.co.ctc_g.eim.app.document.business.domain.DocumentAttributeTypeLayoutDomain;
import jp.co.ctc_g.eim.app.document.business.domain.DocumentObjectDomain;
import jp.co.ctc_g.eim.app.document.business.service.DocumentFormService;
import jp.co.ctc_g.eim.app.form.business.domain.AttributeTypeLayoutDomain;
import jp.co.ctc_g.eim.app.form.business.domain.FormDomain;
import jp.co.ctc_g.eim.app.form.business.service.impl.FormServiceImpl;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.AttributeTypeCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.MultipleCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.ObjectCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.RelationCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessHistoryDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessRoleTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.CodeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FormatDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.OtherNameDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RelationDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RelationTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.StatusDomain;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionGroup;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionIn;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchOperatorEnum;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject.PsedoAttributeTypeEnum;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject.SearchConditionBuildHelper;
import jp.co.ctc_g.eim.framework2.business.service.AccessHistoryService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.business.service.RelationService;
import jp.co.ctc_g.eim.framework2.business.service.RelationTypeService;
import jp.co.ctc_g.eim.framework2.common.enumeration.ValueTypeEnum;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.common.util.ResourceUtils;

public class DocumentFormServiceImpl extends FormServiceImpl implements DocumentFormService {
	/** リレーションタイプサービス */
	private RelationTypeService relationTypeService;
	/** リレーションサービス */
	private RelationService relationService;
	/** アクセス履歴サービス */
	private AccessHistoryService accessHistoryService;

	/** オブジェクトサービス (取得項目なし) */
	private ObjectService objectServiceWithoutAttribute;
	
	/**
	 * @param form
	 * @return
	 * @throws Exception
	 * @see jp.co.ctc_g.eim.app.form.business.service.FormService#create(jp.co.ctc_g.eim.app.form.business.domain.FormDomain)
	 */
	public FormDomain create(FormDomain form) throws Exception {

		// サムネイル&プレビュー変換処理との競合を回避する
		objectServiceWithoutAttribute.getByIdForUpdate(form.getId());

		// 登録済みの帳票を取得する
		FormDomain originalForm = super.getById(form.getId());

		// app.form.dev:帳票タイプフォルダIDを除外する
		for (AttributeDomain attribute : form.getAttributeList()) {
			if (attribute.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTRIBUTE_TYPE_NAME_FORM_TYPE_FOLDER_ID"))) {
				form.getAttributeList().remove(attribute);
				break;
			}
		}
		
		// 入力された属性値を登録済みの帳票に設定する
		for(AttributeDomain attribute: form.getAttributeList()) {
			if(originalForm.getAttribute(attribute.getAttributeType().getDefinitionName()) == null) {
				originalForm.getAttributeList().add(attribute);
			}
		}
		
		// コード型として保持されているリスト値を本来のデータ型に再設定する
		FormDomain newForm = convertCodeToOriginalValue(originalForm);
		
		// 色情報を保存する
		updateDisplayColor(originalForm);
		
		// 関連先文書のアクセス履歴を保存
		updateAccessHistory(originalForm);
		
		// 検索FW更新通知 対象：ドキュメント
		AppUpdateNoticeUtils.updateNoticeInsert(form.getId(), "SEARCHFW_UPDATE_ATTR_DOCUMENT");
		
		// スーパークラスを呼び出す
		return super.update(newForm);
	}
	
	/**
	 * オブジェクトIDをキーにFormDomainを取得します。<br>
	 * その際、値リストに対応します。値リストが存在する場合コード型の属性としてその値を設定します。
	 * 
	 * @param id
	 * @return
	 * @throws Exception
	 * @see jp.co.ctc_g.eim.app.form.business.service.FormService#getById(long)
	 */
	public FormDomain getById(long id) throws Exception {
		
		// スーパークラスを呼び出す
		FormDomain form = super.getById(id);
		
		// 最新リビジョン関連付けフラグtrueのタイプの関連ドキュメントを最新に設定
		updateRelationDocumentAttributeToLatest(form);
		
		// 読み取り権限のない関連ドキュメントを除外
		filterReadAuthority(form);
		
		// attributeTypeLayoutDomainのリストを取得しMapに格納する
		List<AttributeTypeLayoutDomain> attributeTypeLayoutDomainList = form.getFormLayout().getObjectTypeLayout().getAttributeLayoutList();
		Map<Long, DocumentAttributeTypeLayoutDomain> attributeTypeLayoutDomainMap = new HashMap<Long, DocumentAttributeTypeLayoutDomain>();
		
		for (AttributeTypeLayoutDomain attributeTypeLayoutDomain : attributeTypeLayoutDomainList) {
			// 属性タイプIDをキーにattributeLayoutDomainをMapに格納する
			attributeTypeLayoutDomainMap.put(attributeTypeLayoutDomain.getId(), (DocumentAttributeTypeLayoutDomain)attributeTypeLayoutDomain);
		}
		
		// 親フォルダを取得する
		ObjectDomain parent = getParent(form);
		
		// 下位引継ぎ属性を設定する
		if (parent != null)
			setSuccessionAttr(form, parent);
		
		//
		// コード型の属性値として値を保持する
		//
		
		// 言語IDを取得する
		String langId = (String)EIMThreadContext.getTransactionContext().getLangId();
		
		List<AttributeDomain> attributeDomainList = form.getAttributeList();
		
		EIMSession sess = EIMThreadContext.getEIMSession();
		
		for (AttributeDomain attributeDomain : attributeDomainList) {
			
			DocumentAttributeTypeLayoutDomain attributeTypeLayoutDomain = attributeTypeLayoutDomainMap.get(new Long(attributeDomain.getAttributeType().getId()));
			
			// 値リストが存在する場合はDocumentObjectTypeLayoutServiceにてコード型の属性タイプとして事前に設定されている
			if (attributeTypeLayoutDomain.getValueType() == ValueTypeEnum.CODE && attributeTypeLayoutDomain.isSuccessionFlag() == false) {
				// 属性値を取得する
				List<String> attributeValueList = new ArrayList<String>();
				switch(attributeDomain.getAttributeType().getValueType()) {
					case LONG:
						for (long longValue : attributeDomain.getLongList()) {
							attributeValueList.add(String.valueOf(longValue));
						}
						attributeDomain.getLongList().clear();
						break;
					case STRING:
						for (String stringValue : attributeDomain.getStringList()) {
							attributeValueList.add(stringValue);
						}
						attributeDomain.getStringList().clear();
						break;
					case DATE:
						for (Date dateValue : attributeDomain.getDateList()) {
							// Date→文字列変換
							Date convertedDateValue = new Date(DateUtils.convDBTzToCLTzTime(sess, dateValue));
							String stringValue = StringUtils.getDateStringByFormat(convertedDateValue, ResourceUtils.getByKey("EIM.FORMAT.DATE"));
							attributeValueList.add(stringValue);
						}
						attributeDomain.getDateList().clear();
						break;
					case TEXT:
						for (String textValue : attributeDomain.getTextList()) {
							attributeValueList.add(textValue);
						}
						attributeDomain.getTextList().clear();
						break;
					case DOUBLE:
						for (double doubleValue : attributeDomain.getDoubleList()) {
							attributeValueList.add(String.valueOf(doubleValue));
						}
						attributeDomain.getDoubleList().clear();
						break;
				}
				
				// コード型として設定値をコピーする
				for (String value : attributeValueList) {
					CodeDomain valueCodeDomain = new CodeDomain();
					
					// コード値
					valueCodeDomain.setCode(value);
					
					// 名前リスト
					OtherNameDomain otherNameDomain = new OtherNameDomain();
					otherNameDomain.setLangId(langId);
					otherNameDomain.setName(value);
					valueCodeDomain.getNameList().add(otherNameDomain);
					
					attributeDomain.getCodeList().add(valueCodeDomain);
				}
				
				// データ型をコード型に変更する
				attributeDomain.getAttributeType().setValueType(ValueTypeEnum.CODE);
			}
		}
		
		// 権限情報を追加する (FormServiceでは現在ステータスにアサイン先がいない場合は権限が設定されないため)
		StatusDomain currentStatus = form.getStatus();
		if (currentStatus != null && currentStatus.getAssignmentList().size() == 0) {
			// 当該帳票に対してセッションユーザが指定された権限を持つかどうか判定し、付与する
			for (AccessRoleTypeDomain accessRoleType : this.getAccessRoleTypeListForGetById()) {
				if (this.getObjectService().authorized(form, accessRoleType)) {
					form.getAccessRoleTypeList().add(accessRoleType);
				}
			}
		}
		
		// 公開情報を付加する（公開アイコン表示用）
		updateRelationDocumentAttributeForPublic(form);
		
		// オブジェクト型属性に設定されたオブジェクトの属性情報を削除する(レスポンスのデータ量削減のため)
		for (AttributeDomain attributeDomain : attributeDomainList) {
			AttributeTypeDomain attributeTypeDomain = attributeDomain.getAttributeType();
			if (attributeTypeDomain.getValueType() == ValueTypeEnum.OBJECT) {
				List<ObjectDomain> attributeObjectList = attributeDomain.getObjectList();
				if (attributeObjectList != null) {
					for (ObjectDomain attributeObject : attributeObjectList) {
						if (attributeObject.getAttributeList() != null) {
							attributeObject.getAttributeList().clear();
						}
					}
				}
			}
		}
		
		return form;
	}

	
	/**
	 * オブジェクトIDをキーにFormDomainを取得します。<br>
	 * その際、値リストに対応します。値リストが存在する場合コード型の属性としてその値を設定します。
	 *
	 * @param id
	 * @return
	 * @throws Exception
	 * @see jp.co.ctc_g.eim.app.form.business.service.FormService#getById(long)
	 */
	public FormDomain getByIdAdmin(long id) throws Exception {
	
		// スーパークラスを呼び出す
		FormDomain form = super.getById(id);
		
		// 最新リビジョン関連付けフラグtrueのタイプの関連ドキュメントを最新に設定
		updateRelationDocumentAttributeToLatest(form);
		
		// 読み取り権限のない関連ドキュメントを除外
		filterReadAuthority(form);
		
		// attributeTypeLayoutDomainのリストを取得しMapに格納する
		List<AttributeTypeLayoutDomain> attributeTypeLayoutDomainList = form.getFormLayout().getObjectTypeLayout().getAttributeLayoutList();
		Map<Long, DocumentAttributeTypeLayoutDomain> attributeTypeLayoutDomainMap = new HashMap<Long, DocumentAttributeTypeLayoutDomain>();
		
		for (AttributeTypeLayoutDomain attributeTypeLayoutDomain : attributeTypeLayoutDomainList) {
			// 属性タイプIDをキーにattributeLayoutDomainをMapに格納する
			attributeTypeLayoutDomainMap.put(attributeTypeLayoutDomain.getId(), (DocumentAttributeTypeLayoutDomain)attributeTypeLayoutDomain);
		}
		
		// 親フォルダを取得する
		ObjectDomain parent = getParent(form);
		
		// 下位引継ぎ属性を設定する
		if (parent != null)
			setSuccessionAttr(form, parent);
		
		//
		// コード型の属性値として値を保持する
		//
		
		// 言語IDを取得する
		String langId = (String)EIMThreadContext.getTransactionContext().getLangId();
		
		List<AttributeDomain> attributeDomainList = form.getAttributeList();
		
		EIMSession sess = EIMThreadContext.getEIMSession();
		
		for (AttributeDomain attributeDomain : attributeDomainList) {
			
			DocumentAttributeTypeLayoutDomain attributeTypeLayoutDomain = attributeTypeLayoutDomainMap.get(new Long(attributeDomain.getAttributeType().getId()));
			
			// 値リストが存在する場合はDocumentObjectTypeLayoutServiceにてコード型の属性タイプとして事前に設定されている
			if (attributeTypeLayoutDomain.getValueType() == ValueTypeEnum.CODE && attributeTypeLayoutDomain.isSuccessionFlag() == false) {
				// 属性値を取得する
				List<String> attributeValueList = new ArrayList<String>();
				switch(attributeDomain.getAttributeType().getValueType()) {
					case LONG:
						for (long longValue : attributeDomain.getLongList()) {
							attributeValueList.add(String.valueOf(longValue));
						}
						attributeDomain.getLongList().clear();
						break;
					case STRING:
						for (String stringValue : attributeDomain.getStringList()) {
							attributeValueList.add(stringValue);
						}
						attributeDomain.getStringList().clear();
						break;
					case DATE:
						for (Date dateValue : attributeDomain.getDateList()) {
							// Date→文字列変換
							Date convertedDateValue = new Date(DateUtils.convDBTzToCLTzTime(sess, dateValue));
							String stringValue = StringUtils.getDateStringByFormat(convertedDateValue, ResourceUtils.getByKey("EIM.FORMAT.DATE"));
							attributeValueList.add(stringValue);
						}
						attributeDomain.getDateList().clear();
						break;
					case TEXT:
						for (String textValue : attributeDomain.getTextList()) {
							attributeValueList.add(textValue);
						}
						attributeDomain.getTextList().clear();
						break;
					case DOUBLE:
						for (double doubleValue : attributeDomain.getDoubleList()) {
							attributeValueList.add(String.valueOf(doubleValue));
						}
						attributeDomain.getDoubleList().clear();
						break;
				}

				// コード型として設定値をコピーする
				for (String value : attributeValueList) {
					CodeDomain valueCodeDomain = new CodeDomain();
					
					// コード値
					valueCodeDomain.setCode(value);
					
					// 名前リスト
					OtherNameDomain otherNameDomain = new OtherNameDomain();
					otherNameDomain.setLangId(langId);
					otherNameDomain.setName(value);
					valueCodeDomain.getNameList().add(otherNameDomain);
					
					attributeDomain.getCodeList().add(valueCodeDomain);
				}
				
				// データ型をコード型に変更する
				attributeDomain.getAttributeType().setValueType(ValueTypeEnum.CODE);
			}
		}
		
		// 権限情報を追加する (システム管理から呼び出された際、常に取得する必要があるため)
		AccessRoleTypeDomain accessRoleType = new AccessRoleTypeDomain("UPDATE");
		form.getAccessRoleTypeList().add(accessRoleType);
		
		// 公開情報を付加する（公開アイコン表示用）
		updateRelationDocumentAttributeForPublic(form);
		
		return form;
	}
	
	/**
	 * @param form
	 * @return
	 * @throws Exception
	 * @see jp.co.ctc_g.eim.app.form.business.service.FormService#update(jp.co.ctc_g.eim.app.form.business.domain.FormDomain)
	 */
	public FormDomain update(FormDomain form) throws Exception {
		
		// リスト属性の場合の登録値チェック
		checkValueIncludeInListDef(form);

		// DBからフォームを取得する (リスト値はコード型に変換されている)
		FormDomain originalForm = super.getById(form.getId());
		
		// 親フォルダを取得する
		ObjectDomain parent = getParent(form);
		
		// 下位引継ぎ属性フラグを設定する
		if (parent != null)
			setSuccessionFlag(originalForm, parent);
		
		// 下位引継ぎ属性はDBから取得した属性に置き換える
		List<AttributeTypeLayoutDomain> attrLayoutList = originalForm.getFormLayout().getObjectTypeLayout().getAttributeLayoutList();
		for(AttributeTypeLayoutDomain attrLayout: attrLayoutList) {
			if(((DocumentAttributeTypeLayoutDomain)attrLayout).isSuccessionFlag()) {
				String attrTypeDefName = attrLayout.getDefinitionName();
				// 親フォルダの下位引継ぎ属性値が空欄以外の場合、DBから取得した属性に置き換える
				//（親フォルダの下位引継ぎ属性値が空欄の場合、対象の属性がnullであるため置き換えしない）
				if (originalForm.getAttribute(attrTypeDefName) != null) {
					form.getAttributeList().remove(form.getAttribute(attrTypeDefName));
					form.getAttributeList().add(originalForm.getAttribute(attrTypeDefName));
				}
			}
		}
		
		// コード型として保持されているリスト値を本来のデータ型に再設定する
		FormDomain newForm = convertCodeToOriginalValue(form);
		
		// 色情報を保存する
		updateDisplayColor(form);
		
		// 関連先文書のアクセス履歴を保存する
		updateAccessHistory(form, originalForm);
		
		// 自身のアクセス履歴を保存する
		AccessHistoryDomain accessHistory = new AccessHistoryDomain();
		accessHistory.setAction("EIM.ACCESS.TYPE.ATTRIBUTEUPDATE");
		accessHistoryService.create(form, accessHistory);
		
		// 検索FW更新通知 対象：ドキュメント
		AppUpdateNoticeUtils.updateNoticeInsert(form.getId(), "SEARCHFW_UPDATE_ATTR_DOCUMENT");
		
		newForm.setStatus(null);
		
		return super.update(newForm);
	}
	
	/**
	 * 入力規則がリスト定義の属性の登録値がリスト定義内に存在するかどうかをチェックする。
	 *
	 * @param documentDomain
	 * @return
	 * @throws Exception
	 */
	private void checkValueIncludeInListDef(FormDomain formDomain) throws Exception{

		EIMSession sess = jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession();

		for (AttributeDomain attrDomain : formDomain.getAttributeList()) {
			
			CodeDomain CheckcodeDomain = attrDomain.getCode();
			if(CheckcodeDomain == null){
				continue;
			}
			
			AttributeValueMaster attributeValueMaster = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrDomain.getAttributeType().getId());
			if (!Objects.isNull(attributeValueMaster)) {
				for (CodeDomain codeDomain: attrDomain.getCodeList()) {
					String code = codeDomain.getCode();	
					
					EIMAttributeType  AttributeType = attributeValueMaster.getType();
					EIMValueType ValueType = AttributeType.getValueType();
					int atttypeid = ValueType.getId();

					//数値型の場合
					if(atttypeid == 1){ 
						List<Long> masterLongList = new ArrayList<Long>();
						for (long val: attributeValueMaster.getInts()) {
							masterLongList.add(val);
						}
						long long_code = Long.parseLong(code);
						if (!masterLongList.contains(long_code)) {
							throw new EIMException("EIM.ERROR.LOGIC.ATTRIBUTE.VALUETYPE.LIST.VALUE.ILLEGAL",attrDomain.getAttributeType().getDefinitionName(),code);
						}
					//STRING型の場合(文字列)
					}else if(atttypeid == 2){ 
						if (!Arrays.asList(attributeValueMaster.getStrings()).contains(code)) {
							throw new EIMException("EIM.ERROR.LOGIC.ATTRIBUTE.VALUETYPE.LIST.VALUE.ILLEGAL",attrDomain.getAttributeType().getDefinitionName(),code);
						}
					//DATE型の場合(日付)
					}else if(atttypeid == 3){
						SimpleDateFormat sdf = new SimpleDateFormat(EIMResource.getMessage(sess, "EIM.FORMAT.DATE"));
						Date date_code = sdf.parse(code);
						if (!Arrays.asList(attributeValueMaster.getDates()).contains(date_code)) {
								throw new EIMException("EIM.ERROR.LOGIC.ATTRIBUTE.VALUETYPE.LIST.VALUE.ILLEGAL",attrDomain.getAttributeType().getDefinitionName(),code);
						}
					//TEXT型の場合(テキスト)
					}else if(atttypeid == 4){ 
						if (!Arrays.asList(attributeValueMaster.getTexts()).contains(code)) {
							throw new EIMException("EIM.ERROR.LOGIC.ATTRIBUTE.VALUETYPE.LIST.VALUE.ILLEGAL",attrDomain.getAttributeType().getDefinitionName(),code);
						}						
					//DOUBLE型の場合(実数)
					}else if(atttypeid == 5){ 
						List<Double> masterDoubleList = new ArrayList<Double>();
						for (double val: attributeValueMaster.getDoubles()) {
							masterDoubleList.add(val);
						}
						double double_code = Double.parseDouble(code);
						if (!masterDoubleList.contains(double_code)) {
							throw new EIMException("EIM.ERROR.LOGIC.ATTRIBUTE.VALUETYPE.LIST.VALUE.ILLEGAL",attrDomain.getAttributeType().getDefinitionName(),code);
						}
					}
				}
			}
		}
	}
	
	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.DocumentFormService#getAllRevisionById(long)
	 * @since Ver 6.6
	 */
	public List<ObjectDomain> getAllRevisionById(long id) throws Exception {

		// 検索条件設定
		SearchSelectObject latestSearchSelectObject = new SearchSelectObject();
		SearchConditionBuildHelper latestHelper = new SearchConditionBuildHelper();
		SearchConditionGroup latestSearchConditionGroup = latestHelper.group(latestHelper.opAnd());
		latestSearchSelectObject.setCondition(latestSearchConditionGroup);

		// 同一リビジョングループ内のすべてのオブジェクト
		StringBuffer sbSameRevGrpObj = new StringBuffer();
		sbSameRevGrpObj.append("select same_revision_group_obj.OID ");
		sbSameRevGrpObj.append("  from EIMVER old_obj_revision_group, ");
		sbSameRevGrpObj.append("       EIMVER same_revision_group_obj ");
		sbSameRevGrpObj.append(" where old_obj_revision_group.VID = same_revision_group_obj.VID ");
		sbSameRevGrpObj.append("   and old_obj_revision_group.OID = " + id);
		latestSearchConditionGroup.addCondition(new SearchConditionIn(
				latestHelper.opAnd(), PsedoAttributeTypeEnum.ID, SearchOperatorEnum.IN, sbSameRevGrpObj.toString()));

		// 返却項目設定
		List<AttributeTypeDomain> resultAttributes = new ArrayList<AttributeTypeDomain>();
		resultAttributes.add(PsedoAttributeTypeEnum.ID);
		latestSearchSelectObject.setResultAttrs(resultAttributes);

		// 検索実行
		List<ObjectDomain> latestObjectList = getObjectService().getList(latestSearchSelectObject, null);
		
		return latestObjectList;
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.DocumentFormService#deleteDocument(java.util.List)
	 * @since Ver 6.6
	 */
	public void deleteDocument(List<FormDomain> documentList) throws Exception {
		
		EIMSession sess = EIMThreadContext.getEIMSession();
		
		// 指定されたドキュメントに紐づく添付ファイルオブジェクトがある場合、削除する
		ObjectCriteria objectCriteria = new ObjectCriteria();
		
		List<Long> objIdList = new ArrayList<Long>();
		for (FormDomain form : documentList) {
			objIdList.add(form.getId());
		}
		if (objIdList.size() == 0) {
			return;
		}
		
		MultipleCriteria<Long> multipleCriteria = new MultipleCriteria<Long>(objIdList);
		objectCriteria.setIds(multipleCriteria);
		
		//削除するオブジェクトの全属性を取得する。
		//objectServiceForAttributeAllの設定は、itemListForMultiTargetMethod,attributeTypeListForMultiTargetMethodともにNULL指定
		//attributeTypeListForMultiTargetMethodにして全属性を取得してから帳票添付ファイルかどうかを判定する必要があり、
		//またattributeTypeListForMultiTargetMethodがNULLの場合は、itemListForMultiTargetMethodをNULL指定しないと属性値が取得できないため
		//(EIMANAGER 開発者マニュアルの表 4-5-III参照)
		List<ObjectDomain> deleteObjectList = super.getObjectServiceForAttributeAll().getList(objectCriteria);

		for (ObjectDomain deleteObject : deleteObjectList) {
			// 添付ファイルオブジェクトを取得
			for (AttributeDomain attribute : deleteObject.getAttributeList()) {
				for (ObjectDomain object : attribute.getObjectList()) {
					if (object.getType().getDefinitionName().equals(ConfigUtils.getByKey("OBJECT_TYPE_NAME_FORM_ATTACH_FILE"))) {
						try {
							// 添付ファイルオブジェクトを削除
							deleteAppendFileObject(object);
						} catch (EIMException e) {
							// 添付ファイルオブジェクトが既に削除されている場合はエラーとしない
							if (e.getMessageKey().equals("EIM.ERROR.LOGIC.OBJECT.NOTFOUND")) {
								continue;
							}
							throw e;
						}
					}
				}
			}
			// 帳票オブジェクト削除
			EIMObjectType objType = new EIMObjectType(deleteObject.getType().getId(), deleteObject.getType().getName(), null); 
			EIMObject object = new EIMObject(deleteObject.getId(), objType, null, 0, false, null, null, null, null, null, null, false, false, null);
			ObjectUtils.deleteObject(sess, object);
		}
	}
	
	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.DocumentFormService#deleteTargetRevision(java.util.List)
	 * @since Ver 6.6
	 */
	public void deleteTargetRevision(List<FormDomain> formList) throws Exception {

		if (formList.size() == 0) {
			// 削除実施
			super.delete(formList);
			return;
		}

		// 削除対象のオブジェクトを取得（各オブジェクトのリビジョングループIDを取得）
		List<ObjectDomain> deletionObjectList = 
				getObjectWithRevisionGroupList(new ArrayList<ObjectDomain>(formList));
		if (deletionObjectList.size() == 0) {
			// 削除実施
			super.delete(formList);
			return;
		}

		// 削除対象のオブジェクトIDのSetを設定
		Set<Long> deletionObjectIdSet = new HashSet<Long>();
		for (ObjectDomain deletionObject : deletionObjectList) {
			deletionObjectIdSet.add(deletionObject.getId());
		}
		
		// 削除後の最新オブジェクトを取得
		Map<Long, ObjectDomain> deletionObjectIdAndLatestObjectAfterDeletedMap = 
				getObjectIdAndLatestObjectAfterDeletedMap(deletionObjectList);

		// 削除対象オブジェクトを属性値に持つ関連先のオブジェクト一覧を取得
		List<ObjectDomain> relatedObjectList = 
				getRelatedObjectList(deletionObjectList);
		if (relatedObjectList.size() == 0) {
			// 削除実施
			super.delete(formList);
			return;
		}

		// 関連先オブジェクトのオブジェクトタイプIDと対応する最新リビジョン関連付けをONに設定した属性タイプIDSetのMapを取得
		Map<Long, Set<Long>> deletionObjectTypeIdAndRelationAttributeTypeIdSetMap = 
				getObjectTypeIdAndRelationAttributeTypeIdSetMap(relatedObjectList);

		// 関連先のオブジェクトから削除対象のオブジェクト属性を削除
		// ただし、最新リビジョン関連付けがONで1つ前のリビジョンが存在する場合は、1つ前のリビジョンのオブジェクトを設定
		boolean isUpdateAttribute = false;
		for (ObjectDomain relatedObject : relatedObjectList) {
			
			// 関連先の全属性をチェック
			List<AttributeDomain> attributeList = relatedObject.getAttributeList();
			for (AttributeDomain attribute : attributeList) {

				if (attribute.getAttributeType().getValueType() != ValueTypeEnum.OBJECT) {
					// OBJECT型以外は対象外
					continue;
				}
				
				// 最新リビジョン関連付けがONの属性かチェック
				boolean isRelationAttributeType = false;
				Set<Long> relationAttributeTypeIdSet = 
						deletionObjectTypeIdAndRelationAttributeTypeIdSetMap.get(relatedObject.getType().getId());
				if (relationAttributeTypeIdSet != null 
						&& relationAttributeTypeIdSet.contains(attribute.getAttributeType().getId())) {
					isRelationAttributeType = true;
				}
				
				// 該当属性の全値をチェック
				List<ObjectDomain> attributeObjectList = attribute.getObjectList();
				for (int i = attributeObjectList.size() - 1; i >= 0; i--) {
					ObjectDomain attributeObject = attributeObjectList.get(i);

					// 削除対象のオブジェクトでない場合は対象外
					if (!deletionObjectIdSet.contains(attributeObject.getId())) {
						continue;
					}
					
					if (isRelationAttributeType 
							&& deletionObjectIdAndLatestObjectAfterDeletedMap.get(attributeObject.getId()) != null) {
						// 最新リビジョン関連付けがONで1つ前のリビジョンが存在する場合は
						// 1つ前のリビジョンのオブジェクトを設定
						attributeObjectList.set(i, deletionObjectIdAndLatestObjectAfterDeletedMap.get(attributeObject.getId()));

					} else {
						// 削除対象のオブジェクト属性を削除
						attributeObjectList.remove(i);
					}
					isUpdateAttribute = true;
				}

			}
			if (isUpdateAttribute) {
				getObjectService().update(relatedObject);
			}
		}
	
		// 削除実施
		super.delete(formList);
	}

	//==================================
	// Privateメソッド
	//==================================

	/**
	 * 値リストに対応するためにコード型として格納されている属性値を本来のデータ型に変換します。
	 * @param form
	 * @return
	 * @throws Exception
	 */
	private FormDomain convertCodeToOriginalValue(FormDomain form) throws Exception {
		
		FormDomain newForm = form.clone();
		
		// 属性タイプ一覧をDBから取得しMapに格納する
		AttributeTypeCriteria attributeTypeCriteria = new AttributeTypeCriteria();
		attributeTypeCriteria.setObjectTypeId(newForm.getType().getId());
		attributeTypeCriteria.setIncludingParents(true);
		
		List<AttributeTypeDomain> attributeTypeList = getAttributeTypeService().getList(attributeTypeCriteria);
		Map<Long, AttributeTypeDomain> attributeTypeMap = new HashMap<Long, AttributeTypeDomain>();
		
		for (AttributeTypeDomain attributeType : attributeTypeList) {
			// 属性タイプIDをキーにMapに格納する
			attributeTypeMap.put(new Long(attributeType.getId()), attributeType);
		}
		
		EIMSession sess = EIMThreadContext.getEIMSession();
		
		// コード型からオリジナルのバリュータイプに変換する
		for (AttributeDomain attribute : newForm.getAttributeList()) {
			if (attribute.getAttributeType().getValueType() == ValueTypeEnum.CODE) {
				// オリジナルを取得する
				AttributeTypeDomain originalAttributeType = attributeTypeMap.get(attribute.getAttributeType().getId());
				
				// バリュータイプを設定する
				attribute.getAttributeType().setValueType(originalAttributeType.getValueType());
				
				// 属性値を設定する
				for (CodeDomain code : attribute.getCodeList()) {
					switch(attribute.getAttributeType().getValueType()) {
						case LONG:
							attribute.getLongList().add(Long.valueOf(code.getCode()));
							break;
						case STRING:
							attribute.getStringList().add(code.getCode());
							break;
						case DATE:
							// 文字列→Date変換
							Date dateValue = StringUtils.getDateFromString(code.getCode(), ResourceUtils.getByKey("EIM.FORMAT.DATE"));
							Date convertedDateValue = new Date(DateUtils.convCLTzToDBTzTime(sess, dateValue));
							attribute.getDateList().add(convertedDateValue);
							break;
						case TEXT:
							attribute.getTextList().add(code.getCode());
							break;
						case DOUBLE:
							attribute.getDoubleList().add(Double.valueOf(code.getCode()));
							break;
					}
				}
				
				// コード型属性を削除する
				attribute.getCodeList().clear();
			}
		}
		
		return newForm;
	}
	
	/**
	 * 値リストに対応した色オブジェクトを更新します。
	 * @param form
	 * @throws Exception
	 */
	private void updateDisplayColor(FormDomain form) throws Exception {
		EIMSession sess = EIMThreadContext.getEIMSession();
		
		for (AttributeDomain attribute : form.getAttributeList()) {
			if (attribute.getAttributeType().getValueType() == ValueTypeEnum.CODE) {
				// 色オブジェクトを更新する
				EIMObject obj = new EIMObject(form.getId(), null, null, 0, false, null, null, null, null, null, null, false, false, null);
				EIMAttributeType attrType = new EIMAttributeType(attribute.getAttributeType().getId(), null, null);
				String value = null;
				CodeDomain code =attribute.getCode();
				if (code != null)
					value = code.getCode();
				
				DisplayColorUtil.updateDisplayColor(sess, obj, attrType, value);
			}
		}
	}
	
	/**
	 * 親フォルダを取得します。
	 * @param form 文書
	 * @return 親フォルダ
	 */
	private ObjectDomain getParent(FormDomain form) throws Exception {
		// ドキュメントリレーション取得
		RelationTypeDomain relType = relationTypeService.getByDefinitionName(ConfigUtils.getByKey("RELATION_TYPE_NAME_DOCUMENT"));
		
		// 親ワークスペース、または、フォルダ取得
		RelationCriteria criteria = new RelationCriteria();
		criteria.setChildObjectId(form.getId());
		criteria.setRelationTypeId(relType.getId());
		List<RelationDomain> relList = relationService.getList(criteria);
		if(relList == null || relList.size() == 0) {
			// リレーションが取得できない(=過去版)の場合、処理終了
			return null;
		}
		ObjectDomain parent = relList.get(0).getParent();
		
		return parent;
	}
	
	/**
	 * 下位引継ぎ属性フラグを設定します。
	 * @param form 文書
	 * @param parent 親フォルダ
	 */
	private void setSuccessionFlag(FormDomain form, ObjectDomain parent) throws Exception {
		// 下位引継ぎ対象の属性タイプIDリスト
		Set<Long> successionIdSet = new HashSet<Long>();
		AttributeDomain successionAttr = parent.getAttribute(ConfigUtils.getByKey("ATTR_NAME_FOLDER_TO_LOW_ATTR"));
		if(successionAttr != null) {
			successionIdSet = new HashSet<Long>(successionAttr.getLongList());
		}
		
		// 属性タイプレイアウト情報に下位引継ぎ属性情報を設定
		if(form.getFormLayout() != null) {
			for(AttributeTypeLayoutDomain attrLayout: form.getFormLayout().getObjectTypeLayout().getAttributeLayoutList()) {
				if(successionIdSet.contains(attrLayout.getId())) {
					// 下位引継ぎ属性フラグをON
					((DocumentAttributeTypeLayoutDomain)attrLayout).setSuccessionFlag(true);
				}	
			}
		}
	}
	
	/**
	 * 下位引継ぎ属性フラグおよびそれに対応するUIコントロール、バリュータイプ、値を設定します。
	 * @param form 文書
	 * @param parent 親フォルダ
	 */
	private void setSuccessionAttr(FormDomain form, ObjectDomain parent) throws Exception {
		// 下位引継ぎ属性フラグを設定する
		setSuccessionFlag(form, parent);
		
		EIMSession sess = EIMThreadContext.getEIMSession();
		
		// 属性タイプレイアウト情報に下位引継ぎ属性情報を設定
		if(form.getFormLayout() != null) {
			for(AttributeTypeLayoutDomain attrLayout: form.getFormLayout().getObjectTypeLayout().getAttributeLayoutList()) {
				if (((DocumentAttributeTypeLayoutDomain)attrLayout).isSuccessionFlag() == true) {
					// 下位引継ぎ属性フラグをON
					((DocumentAttributeTypeLayoutDomain)attrLayout).setSuccessionFlag(true);
					
					// 属性の初期値を設定
					AttributeDomain attr = parent.getAttribute(attrLayout.getDefinitionName());
					if(attr == null) continue;
					
					
					// 下位引継ぎ属性(親の属性)のデータ型に応じてUIコントロール、データ型、属性値を設定する (コード型に変換済みのものがあるため上書きする)
					switch(attr.getAttributeType().getValueType()) {
						case LONG:
							attrLayout.setUiControlId(ConfigUtils.getByKey("UI_CONTROL_ID_TEXT_INPUT"));
							attrLayout.setUiControlType(ConfigUtils.getByKey("UI_CONTROL_TYPE_TEXT_INPUT"));
							attrLayout.setValueType(ValueTypeEnum.LONG);
							break;
						case STRING:
							attrLayout.setUiControlId(ConfigUtils.getByKey("UI_CONTROL_ID_TEXT_INPUT"));
							attrLayout.setUiControlType(ConfigUtils.getByKey("UI_CONTROL_TYPE_TEXT_INPUT"));
							attrLayout.setValueType(ValueTypeEnum.STRING);
							break;
						case DATE:
							// 日付を文字列として表示
							List<String> dateStrList = new ArrayList<String>();
							AttributeDomain attribute = form.getAttribute(attrLayout.getDefinitionName());
							for(Date date: attribute.getDateList()) {
								Date convertedDateValue = new Date(DateUtils.convDBTzToCLTzTime(sess, date));
								String stringValue = StringUtils.getDateStringByFormat(convertedDateValue, ResourceUtils.getByKey("EIM.FORMAT.DATE"));
								dateStrList.add(stringValue);
							}
							attrLayout.setUiControlId(ConfigUtils.getByKey("UI_CONTROL_ID_TEXT_INPUT"));
							attrLayout.setUiControlType(ConfigUtils.getByKey("UI_CONTROL_TYPE_TEXT_INPUT"));
							attrLayout.setValueType(ValueTypeEnum.STRING);
							attribute.setStringList(dateStrList);
							attribute.getAttributeType().setValueType(ValueTypeEnum.STRING);
							attribute.getDateList().clear();
							break;
						case TEXT:
							break;
						case DOUBLE:
							attrLayout.setUiControlId(ConfigUtils.getByKey("UI_CONTROL_ID_TEXT_INPUT"));
							attrLayout.setUiControlType(ConfigUtils.getByKey("UI_CONTROL_TYPE_TEXT_INPUT"));
							attrLayout.setValueType(ValueTypeEnum.DOUBLE);
							break;
						case OBJECT:
							// 親オブジェクトにオブジェクト型属性は設定できないため未実装
							break;
						case USER:
							// 親オブジェクトにユーザ型属性は設定できないため未実装
							break;
						case CODE:
							// 親オブジェクトにコード型属性は設定できないため未実装
							break;
					}
				}
			}
		}
	}
	
	/**
	 * 関連ドキュメントのアクセス履歴更新の処理
	 * @param form 文書
	 */
	private void updateAccessHistory(FormDomain form) throws Exception {
		updateAccessHistory(form, null);
	}
	
	/**
	 * 関連ドキュメントのアクセス履歴更新の処理
	 * @param form 文書
	 * @param beforeForm 更新前文書
	 */
	private void updateAccessHistory(FormDomain form, FormDomain beforeForm) throws Exception {
		
		// 関連ドキュメント属性の属性定義名称リスト
		List<String> attrTypeDefNameList = new ArrayList<String>();
		List<AttributeTypeLayoutDomain> attrLayoutList = form.getFormLayout().getObjectTypeLayout().getAttributeLayoutList();
		for(AttributeTypeLayoutDomain attrLayout: attrLayoutList) {
			String uiControlId = attrLayout.getUiControlId();
			if(uiControlId != null && uiControlId.equals(ConfigUtils.getByKey("UI_CONTROL_ID_RELATED_DOCUMEMT"))) {
				attrTypeDefNameList.add(attrLayout.getDefinitionName());
			}
		}
		
		// 関連ドキュメント属性分ループ
		for(String attrTypeDefName: attrTypeDefNameList) {
			
			// 保存前オブジェクトの属性値リスト
			List<ObjectDomain> beforeValList = new ArrayList<ObjectDomain>();
			if(beforeForm != null && beforeForm.getAttribute(attrTypeDefName) != null) {
				beforeValList = beforeForm.getAttribute(attrTypeDefName).getObjectList();
			}
			
			// 保存後オブジェクトの属性値リスト
			List<ObjectDomain> afterValList = new ArrayList<ObjectDomain>();
			if(form.getAttribute(attrTypeDefName) != null) {
				afterValList = form.getAttribute(attrTypeDefName).getObjectList();
			}
			
			// 保存前にのみ存在するオブジェクトリスト
			List<ObjectDomain> beforeDiffList = getDiffList(beforeValList, afterValList);
			for(ObjectDomain beforeDiff: beforeDiffList) {
				// アクセス履歴(関連解除)登録
				AccessHistoryDomain accessHistory = new AccessHistoryDomain();
				accessHistory.setAction("EIM.ACCESS.TYPE.DOCREL.DELETE" + ConfigUtils.getByKey("ACCESS_HISTORY_DELIMITER") + form.getName());
				accessHistoryService.create(beforeDiff, accessHistory);
			}
			
			// 保存後にのみ存在するオブジェクトリスト
			List<ObjectDomain> afterDiffList = getDiffList(afterValList, beforeValList);
			for(ObjectDomain afterDiff: afterDiffList) {
				// アクセス履歴(関連付け)登録
				AccessHistoryDomain accessHistory = new AccessHistoryDomain();
				accessHistory.setAction("EIM.ACCESS.TYPE.DOCREL.CREATE" + ConfigUtils.getByKey("ACCESS_HISTORY_DELIMITER") + form.getName());
				accessHistoryService.create(afterDiff, accessHistory);
			}
		}
	}
	
	/**
	 * 差分リスト取得の処理
	 * @param targetList 対象リスト
	 * @param compareList 比較リスト
	 * @return 差分リスト
	 */
	private List<ObjectDomain> getDiffList(List<ObjectDomain> targetList, List<ObjectDomain> compareList) {
		
		// 比較対象オブジェクトIDリスト
		Set<Long> compareIdSet = new HashSet<Long>();
		for(ObjectDomain compare: compareList) {
			compareIdSet.add(compare.getId());
		}
		
		// 返却リスト
		List<ObjectDomain> rtList = new ArrayList<ObjectDomain>();
		for(ObjectDomain target: targetList) {
			if(!compareIdSet.contains(target.getId())) {
				rtList.add(target);
			}
		}
		
		return rtList;
	}
	
	/**
	 * 最新リビジョン関連付けフラグtrueの属性タイプの関連ドキュメントを最新リビジョンに変更します。
	 * @param form フォームドメイン
	 * @since Ver6.6
	 */
	protected void updateRelationDocumentAttributeToLatest(FormDomain form) throws Exception {
		
		// 最新リビジョン関連付け対象の属性タイプを取得
		List<AttributeTypeLayoutDomain> docAttributeTypeLayoutList = form.getFormLayout().getObjectTypeLayout().getAttributeLayoutList();

		// 最新化対象の属性タイプIDのSetを作成
		Set<Long> targetAttributeTypeIdSet = new HashSet<Long>();
		for (AttributeTypeLayoutDomain docAttributeTypeLayout : docAttributeTypeLayoutList) {
			
			// 最新リビジョン関連付け対象かチェック
			if (!((DocumentAttributeTypeLayoutDomain)docAttributeTypeLayout).isRelationFlag()) {
				continue;
			}
			
			targetAttributeTypeIdSet.add(docAttributeTypeLayout.getId());
		}
		
		// 対象なし
		if (targetAttributeTypeIdSet.size() == 0) {
			return;
		}

		// 最新化対象関連ドキュメント属性のSetを作成
		// 最新化対象関連ドキュメントIDのSetを作成
		Set<AttributeDomain> relDocAttributeSet = new HashSet<AttributeDomain>();
		Set<ObjectDomain> oldRelDocSet = new HashSet<ObjectDomain>();
		for (AttributeDomain attribute : form.getAttributeList()) {
			// 最新リビジョン関連付け対象かチェック
			if (!targetAttributeTypeIdSet.contains(attribute.getAttributeType().getId())) {
				continue;
			}
			
			relDocAttributeSet.add(attribute);
			
			for (ObjectDomain relDocObject : attribute.getObjectList()) {
				oldRelDocSet.add(relDocObject);
			}
		}
		
		// 対象なし
		if (oldRelDocSet.size() == 0) {
			return;
		}

		// 関連ドキュメントを取得する。リビジョングループIDのみ設定される。
		List<ObjectDomain> oldRelDocObjecWithRevisionGroupOnlytList = 
				getObjectWithRevisionGroupList(new ArrayList<ObjectDomain>(oldRelDocSet));

		// 各関連ドキュメントの最新版を取得する
		Map<Long, ObjectDomain> oldObjectIdAndLatestRelDocObjectMap = 
				getObjectIdAndLatestObjectMap(new ArrayList<ObjectDomain>(oldRelDocObjecWithRevisionGroupOnlytList));

		// 対象なし
		if (oldObjectIdAndLatestRelDocObjectMap.size() == 0) {
			return;
		}

		
		// 各関連ドキュメントを最新に置き換え
		for (AttributeDomain attribute : relDocAttributeSet) {
			int relObjSize = attribute.getObjectList().size();
			Set<Long> relDocIdSet = new HashSet<>();
			List<ObjectDomain> duplicateDocList = new ArrayList<>();
			for (int i = 0; i < relObjSize; i++) {
				ObjectDomain relDocObject = attribute.getObjectList().get(i);
				
				// 最新化対象オブジェクト以外は対象外
				if (!oldObjectIdAndLatestRelDocObjectMap.containsKey(relDocObject.getId())) {
					relDocIdSet.add(relDocObject.getId());
					continue;
				}
				
				// 最新版ドキュメントを取得する
				ObjectDomain latenstRelDocObject = oldObjectIdAndLatestRelDocObjectMap.get(relDocObject.getId());
				
				// 重複チェック
				if (!relDocIdSet.contains(latenstRelDocObject.getId())) {
					// 重複していなければ最新の関連ドキュメントに更新する
					attribute.getObjectList().set(i, latenstRelDocObject.clone());
					relDocIdSet.add(latenstRelDocObject.getId());
				} else {
					// 重複している場合、保管して後で削除する
					duplicateDocList.add(relDocObject);
				}
			}
			
			// 重複データを削除する
			attribute.getObjectList().removeAll(duplicateDocList);
		}

	}

	/**
	 * オブジェクトのリストを取得します。リビジョングループIDを設定します。
	 * @param objectList オブジェクトのリスト
	 * @return オブジェクトのリスト
	 * @since Ver6.6
	 */
	private List<ObjectDomain> getObjectWithRevisionGroupList(
			List<ObjectDomain> objectList) throws Exception {

		if (objectList.size() == 0) {
			return new ArrayList<ObjectDomain>();
		}

		Object[] ids = new Object[objectList.size()];
		for (int i = 0; i < objectList.size(); i++) {
			ids[i] = objectList.get(i).getId();
		}

		// 現行の関連ドキュメントを取得
		SearchSelectObject searchSelectObject = new SearchSelectObject();
		SearchConditionBuildHelper helper = new SearchConditionBuildHelper();
		SearchConditionGroup searchConditionGroup = helper.group(helper.opAnd());
		searchSelectObject.setCondition(searchConditionGroup);
		
		// 返却項目設定
		List<AttributeTypeDomain> resultAttributes = new ArrayList<AttributeTypeDomain>();
		resultAttributes.add(PsedoAttributeTypeEnum.REVISION_GROUP_ID);
		searchSelectObject.setResultAttrs(resultAttributes);

		// ID
		searchConditionGroup.addCondition(helper.inArray(
				helper.opAnd(), PsedoAttributeTypeEnum.ID, SearchOperatorEnum.IN, ids));

		// 検索実行
		return getObjectService().getList(searchSelectObject, null);
	}

	/**
	 * オブジェクトIDと対応する最新オブジェクトのMapを取得します。
	 * @param objectList オブジェクトのリスト(それぞれリビジョングループIDが設定されていること)
	 * @return オブジェクトIDと最新オブジェクトのMap
	 * @since Ver6.6
	 */
	private Map<Long, ObjectDomain> getObjectIdAndLatestObjectMap(
			List<ObjectDomain> objectList) throws Exception {

		if (objectList.size() == 0) {
			return new HashMap<Long, ObjectDomain>();
		}

		// 検索条件設定
		SearchSelectObject latestSearchSelectObject = new SearchSelectObject();
		SearchConditionBuildHelper latestHelper = new SearchConditionBuildHelper();
		SearchConditionGroup latestSearchConditionGroup = latestHelper.group(latestHelper.opAnd());
		latestSearchSelectObject.setCondition(latestSearchConditionGroup);

		// 同一リビジョングループ内のすべてのオブジェクト
		StringBuffer sbSameRevGrpObj = new StringBuffer();
		sbSameRevGrpObj.append("select same_revision_group_obj.OID ");
		sbSameRevGrpObj.append("  from EIMVER old_obj_revision_group, ");
		sbSameRevGrpObj.append("       EIMVER same_revision_group_obj ");
		sbSameRevGrpObj.append(" where old_obj_revision_group.VID = same_revision_group_obj.VID ");
		sbSameRevGrpObj.append("   and old_obj_revision_group.OID in ( ");
		sbSameRevGrpObj.append(joinId(objectList, ","));
		sbSameRevGrpObj.append("       )");
		latestSearchConditionGroup.addCondition(new SearchConditionIn(
				latestHelper.opAnd(), PsedoAttributeTypeEnum.ID, SearchOperatorEnum.IN, sbSameRevGrpObj.toString()));

		// 最新版
		latestSearchConditionGroup.addCondition(latestHelper.latest(latestHelper.opAnd()));

		// 検索実行
		List<ObjectDomain> latestObjectList = getObjectService().getList(latestSearchSelectObject, null);
		
		// リビジョングループIDと最新版の関連ドキュメントのMapを作成
		Map<Long, ObjectDomain> revGrpIdAndLatestRelDocObjectMap = new HashMap<Long, ObjectDomain>();
		for (ObjectDomain latestObject : latestObjectList) {
			// latestが1のオブジェクトを取得しているが、チェックアウト中はlatestが1のオブジェクトは改訂中、編集中の2件になる。
			// そのため、revisionをみて最新をmapに設定する。
			ObjectDomain sameRevisionGroupObject = 
					revGrpIdAndLatestRelDocObjectMap.get(latestObject.getRevisionGroupId());
			if (sameRevisionGroupObject == null || sameRevisionGroupObject.getRevision() < latestObject.getRevision()) {
				revGrpIdAndLatestRelDocObjectMap.put(latestObject.getRevisionGroupId(), latestObject);
			}
		}
		
		// オブジェクトIDと対応する最新版オブジェクトのMapを作成
		Map<Long, ObjectDomain> oldObjIdAndLatestObjectMap = new HashMap<Long, ObjectDomain>();
		for (int i = 0; i < objectList.size(); i++) {
			ObjectDomain object = objectList.get(i);
			// 最新オブジェクトが存在しない場合は結果に含めない
			if (!revGrpIdAndLatestRelDocObjectMap.containsKey(object.getRevisionGroupId())) {
				continue;
			}
			oldObjIdAndLatestObjectMap.put(
					object.getId(), 
					revGrpIdAndLatestRelDocObjectMap.get(object.getRevisionGroupId()));
		}
		
		return oldObjIdAndLatestObjectMap;
	}

	/**
	 * 配列を連結します。
	 * @param objects オブジェクトの配列
	 * @param separator セパレータ
	 * @return 連結した文字列
	 * @since Ver6.6
	 */
	private String joinId(List<? extends ObjectDomain> objectList, String separator) {
		StringBuffer sb = new StringBuffer();
		
		for (int i = 0; i < objectList.size(); i++) {
			
			if (i > 0) {
				sb.append(separator);
			}
			sb.append(objectList.get(i).getId());
		}

		return sb.toString();
	}
	
	/**
	 * 削除するオブジェクトIDと削除後の最新のオブジェクト(リビジョングループIDのみ設定)のMapを取得します。
	 * @param objectList オブジェクトのリスト(それぞれリビジョングループIDが設定されていること)
	 * @return 削除するオブジェクトIDと削除後の最新のオブジェクトのMap
	 * @since Ver6.6
	 */
	private Map<Long, ObjectDomain> getObjectIdAndLatestObjectAfterDeletedMap(
			List<ObjectDomain> objectList) throws Exception {

		if (objectList.size() == 0) {
			return new HashMap<Long, ObjectDomain>();
		}

		// 検索条件設定
		SearchSelectObject searchSelectObject = new SearchSelectObject();
		SearchConditionBuildHelper helper = new SearchConditionBuildHelper();
		SearchConditionGroup searchConditionGroup = helper.group(helper.opAnd());
		searchSelectObject.setCondition(searchConditionGroup);

		// 同一リビジョングループ内のすべてのオブジェクト
		StringBuffer sbSameRevGrpObj = new StringBuffer();
		sbSameRevGrpObj.append("select max(same_revision_group_obj.OID) ");
		sbSameRevGrpObj.append("  from EIMVER old_obj_revision_group, ");
		sbSameRevGrpObj.append("       EIMVER same_revision_group_obj ");
		sbSameRevGrpObj.append(" where old_obj_revision_group.VID = same_revision_group_obj.VID ");
		sbSameRevGrpObj.append("   and old_obj_revision_group.OID in ( ");
		sbSameRevGrpObj.append(joinId(objectList, ","));
		sbSameRevGrpObj.append("       )");
		sbSameRevGrpObj.append("   and same_revision_group_obj.OID <> old_obj_revision_group.OID ");
		sbSameRevGrpObj.append(" group by same_revision_group_obj.VID ");
		searchConditionGroup.addCondition(new SearchConditionIn(
				helper.opAnd(), PsedoAttributeTypeEnum.ID, SearchOperatorEnum.IN, sbSameRevGrpObj.toString()));

		// 返却項目設定
		List<AttributeTypeDomain> resultAttributes = new ArrayList<AttributeTypeDomain>();
		resultAttributes.add(PsedoAttributeTypeEnum.REVISION_GROUP_ID);
		searchSelectObject.setResultAttrs(resultAttributes);

		// 検索実行
		List<ObjectDomain> latestObjectList = getObjectService().getList(searchSelectObject, null);
		
		// リビジョングループIDと最新版の関連ドキュメントのMapを作成
		Map<Long, ObjectDomain> revGrpIdAndLatestRelDocObjectMap = new HashMap<Long, ObjectDomain>();
		for (ObjectDomain latestObject : latestObjectList) {
			revGrpIdAndLatestRelDocObjectMap.put(latestObject.getRevisionGroupId(), latestObject);
		}
		
		// オブジェクトIDと対応する最新版オブジェクトのMapを作成
		Map<Long, ObjectDomain> deletionObjIdAndLatestObjectMap = new HashMap<Long, ObjectDomain>();
		for (int i = 0; i < objectList.size(); i++) {
			ObjectDomain object = objectList.get(i);
			// 最新オブジェクトが存在しない場合は結果に含めない
			if (!revGrpIdAndLatestRelDocObjectMap.containsKey(object.getRevisionGroupId())) {
				continue;
			}
			deletionObjIdAndLatestObjectMap.put(
					object.getId(), 
					revGrpIdAndLatestRelDocObjectMap.get(object.getRevisionGroupId()));
		}
		
		return deletionObjIdAndLatestObjectMap;
	}

	/**
	 * オブジェクトタイプIDと対応する最新リビジョン関連付けをONに設定した属性タイプIDSetのMapを取得します。
	 * @param objectList オブジェクトリスト
	 * @return オブジェクトタイプIDと対応する最新リビジョン関連付けをONに設定した属性タイプIDSetのMap
	 * @since Ver6.6
	 */
	private Map<Long, Set<Long>> getObjectTypeIdAndRelationAttributeTypeIdSetMap(List<ObjectDomain> objectList) throws Exception {

		if (objectList.size() == 0) {
			return new HashMap<Long, Set<Long>>();
		}

		// 検索条件設定
		SearchSelectObject searchSelectObject = new SearchSelectObject();
		SearchConditionBuildHelper helper = new SearchConditionBuildHelper();
		SearchConditionGroup searchConditionGroup = helper.group(helper.opAnd());
		searchSelectObject.setCondition(searchConditionGroup);

		// 検索条件：NAMEが取得対象のタイプID
		Set<String> objectTypeIdSet = new HashSet<String>();
		for (ObjectDomain object : objectList) {
			objectTypeIdSet.add(String.valueOf(object.getType().getId()));
		}
		searchConditionGroup.addCondition(new SearchConditionIn(
				helper.opAnd(), PsedoAttributeTypeEnum.NAME, SearchOperatorEnum.IN, 
				objectTypeIdSet.toArray(new String[objectTypeIdSet.size()])));

		// 検索条件：TYPEがオブジェクトタイプ
		ObjectTypeDomain objectType = 
				getObjectTypeService().getByDefinitionName(EIMConfig.get("OBJECT_TYPE_NAME_OBJECTTYPE"));
		searchConditionGroup.addCondition(helper.eq(helper.opAnd(), PsedoAttributeTypeEnum.TYPE, objectType.getId()));

		// 返却項目設定
		List<AttributeTypeDomain> resultAttrs = new ArrayList<AttributeTypeDomain>();
		searchSelectObject.setResultAttrs(resultAttrs);
		
		// 返却項目：最新リビジョン関連付け
		AttributeTypeDomain ralationFlagAttribute = 
				getAttributeTypeService().getByDefinitionName(EIMConfig.get("ATTRIBUTE_TYPE_NAME_RELATION"));
		resultAttrs.add(ralationFlagAttribute);
		resultAttrs.add(SearchSelectObject.PsedoAttributeTypeEnum.NAME);
		
		// 検索実行
		List<ObjectDomain> objectTypeObjectList = getObjectService().getList(searchSelectObject, null);
		
		// オブジェクトタイプIDと対応する最新リビジョン関連付けをONに設定した属性タイプIDSetのMapを作成
		Map<Long, Set<Long>> objTypeIdAndRelationAttributeTypeIdMap = new HashMap<Long, Set<Long>>();
		for (ObjectDomain objectTypeObject : objectTypeObjectList) {
			AttributeDomain relationAttribute = objectTypeObject.getAttribute(EIMConfig.get("ATTRIBUTE_TYPE_NAME_RELATION"));
			if  (relationAttribute == null || relationAttribute.getLongList() == null) {
				continue;
			}
			objTypeIdAndRelationAttributeTypeIdMap.put(
					Long.parseLong(objectTypeObject.getName()), new HashSet<Long>(relationAttribute.getLongList()));
		}
		
		return objTypeIdAndRelationAttributeTypeIdMap;
	}
	
	/**
	 * 対象オブジェクトを属性値に持つ関連先オブジェクトのリストを取得します。
	 * @param objectList オブジェクトリスト
	 * @return 対象オブジェクトを属性値に持つ関連先オブジェクトリスト
	 * @since Ver6.6
	 */
	private List<ObjectDomain> getRelatedObjectList(List<ObjectDomain> objectList) throws Exception {

		if (objectList.size() == 0) {
			return new ArrayList<ObjectDomain>();
		}

		// 検索条件設定
		SearchSelectObject searchSelectObject = new SearchSelectObject();
		SearchConditionBuildHelper helper = new SearchConditionBuildHelper();
		SearchConditionGroup searchConditionGroup = helper.group(helper.opAnd());
		searchSelectObject.setCondition(searchConditionGroup);

		// 対象オブジェクトを属性値に持つオブジェクト
		StringBuffer sb = new StringBuffer();
		sb.append("select attr_obj_ref.ID ");
		sb.append("  from EIMOBJREF attr_obj_ref ");
		sb.append(" where attr_obj_ref.VALUE in ( ");
		sb.append(joinId(objectList, ","));
		sb.append("       )");
		searchConditionGroup.addCondition(new SearchConditionIn(
				helper.opAnd(), PsedoAttributeTypeEnum.ID, SearchOperatorEnum.IN, sb.toString()));

		// 検索実行
		return getObjectService().getList(searchSelectObject, null);
	}

	/**
	 * 関連ドキュメントの公開情報を更新します。
	 * 関連ドキュメント属性のObjectDomainリストをDocumentObjectDomainリストに変換します。
	 * @param form フォーム
	 * @since Ver6.6
	 */
	private void updateRelationDocumentAttributeForPublic(FormDomain form) throws Exception {

		// フォーム内の関連ドキュメント属性を取得
		List<AttributeDomain> relationDocumentAttributeList = getRelationDocumentAttributeList(form);
		
		// 関連ドキュメント属性内のドキュメントオブジェクトのIDとドメインのMapを取得
		Map<Long, DocumentObjectDomain> idAndDocumentObjectMap = 
				getIdAndDocumentObjectMap(relationDocumentAttributeList);
		if (idAndDocumentObjectMap == null) {
			// 対象なし
			return;
		}
		
		// 公開読取権限のみ保有しているオブジェクトIDを取得
		Set<Long> authOnlyPublicReadIdSet = getAuthOnlyPublicReadSet(relationDocumentAttributeList);
		
		List<DocumentObjectDomain> targetObjectList = new ArrayList<DocumentObjectDomain>(idAndDocumentObjectMap.values());
		Set<Long> publicDocumentIdSet = getPublicDocumentIdSetForNoWorkflow(targetObjectList);
		for ( AttributeDomain attribute : relationDocumentAttributeList) {
			
			// 関連ドキュメントのObjectDomainをDocumentObjectDomain（公開情報含む）に変換
			List<DocumentObjectDomain> documentObjects = new ArrayList<DocumentObjectDomain>();
			for ( ObjectDomain object : attribute.getObjectList()) {
				DocumentObjectDomain documentObject = idAndDocumentObjectMap.get(object.getId());
				setPublicFileName(documentObject, publicDocumentIdSet);
				
				// 公開読取権限のみ保有フラグを付与
				if (authOnlyPublicReadIdSet.contains(object.getId())) {
					documentObject.setAuthOnlyPublicRead(true);
				}
				// PDF変換処理実行日時を付与
				AttributeDomain attributePDFConvExecDate = object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PDF_CONV_EXEC_DATE"));
				if (attributePDFConvExecDate != null) {
					documentObject.setPdfConversionExecutedDate(attributePDFConvExecDate.getDate());
				}
				// 公開PDF事前登録日時を付与
				AttributeDomain attributePDFPreRegistDate = object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PDF_PRE_REGIST_DATE"));
				if (attributePDFPreRegistDate != null) {
					documentObject.setPdfPreRegistDate(attributePDFPreRegistDate.getDate());
				}

				documentObjects.add(documentObject);
			}
			attribute.getObjectList().clear();
			attribute.getObjectList().addAll(documentObjects);
		}
	}

	/**
	 * フォーム内の関連ドキュメント属性リストを取得します。
	 * フォームのレイアウト情報のUIコントロールIDから関連ドキュメントを判定します。
	 * @param form フォーム
	 * @return 関連ドキュメントの属性List
	 * @since Ver6.6
	 */
	private List<AttributeDomain> getRelationDocumentAttributeList(FormDomain form) {
		
		// レイアウト情報から関連ドキュメント属性IDのSetを取得
		Set<Long> idSet = new HashSet<Long>();
		for (AttributeTypeLayoutDomain attributeLayout : form.getFormLayout().getObjectTypeLayout().getAttributeLayoutList()) {
			if (!EIMConfig.get("UI_CONTROL_ID_RELATED_DOCUMEMT").equals(attributeLayout.getUiControlId())) {
				continue;
			}
			idSet.add(attributeLayout.getId());
		}
		
		// 関連ドキュメント属性IDから属性を取得しリスト化
		List<AttributeDomain> attributeList = new ArrayList<AttributeDomain>();
		for ( AttributeDomain attribute : form.getAttributeList()) {
			// 関連ドキュメントかチェック
			if (!idSet.contains(attribute.getAttributeType().getId())) {
				continue;
			}
			attributeList.add(attribute);
		}
		return attributeList;
	}
	
	/**
	 * 関連ドキュメント属性内のオブジェクトID、オブジェクトドメインのMapを取得します。
	 * @param attributeList 関連ドキュメント属性のリスト
	 * @return 関連ドキュメント属性内のオブジェクトID、オブジェクトドメインのMap
	 * @since Ver6.6
	 */
	private Map<Long, DocumentObjectDomain> getIdAndDocumentObjectMap(
			List<AttributeDomain> attributeList) throws Exception{

		// 関連ドキュメント属性内のオブジェクト情報を取得する
		MultipleCriteria<Long> docIds = new MultipleCriteria<Long>();
		for ( AttributeDomain attribute : attributeList) {
			
			// 関連ドキュメントの取得対象を設定
			for (ObjectDomain object : attribute.getObjectList()) {
				docIds.add(object.getId());
			}
		}
		
		if (docIds.size() == 0) {
			// 対象なし
			return null;
		}
		ObjectCriteria oc = new ObjectCriteria();
		oc.setIds(docIds);
		List<ObjectDomain> docObjs = getObjectService().getList(oc);

		// 取得データをMap化する
		Map<Long, DocumentObjectDomain> map = new HashMap<Long, DocumentObjectDomain>();
		for ( ObjectDomain object : docObjs) {
			map.put(object.getId(), new DocumentObjectDomain(object));
		}
		
		return map;
	}
	
	/**
	 * WFなしドキュメントにて公開アイコンを表示可能なオブジェクトIDのSetを取得します。
	 * WFなしドキュメントを改訂するとLATEST=1のレコードが2件できる。
	 * そのうちリビジョンが大きいほうは公開アイコンを出力しないためSetには含めない。
	 * @param documentObjects 対象のドキュメントオブジェクト
	 * @return 公開アイコンを表示可能なオブジェクトIDのSet
	 * @since Ver6.6
	 */
	private Set<Long> getPublicDocumentIdSetForNoWorkflow(
			List<DocumentObjectDomain> documentObjects) throws Exception {

		if (documentObjects.size() == 0) {
			return new HashSet<Long>();
		}

		// 検索条件設定
		SearchSelectObject searchSelectObject = new SearchSelectObject();
		SearchConditionBuildHelper helper = new SearchConditionBuildHelper();
		SearchConditionGroup searchConditionGroup = helper.group(helper.opAnd());
		searchSelectObject.setCondition(searchConditionGroup);

		// 同一リビジョングループ内のすべてのオブジェクト
		StringBuffer sbSameRevGrpObj = new StringBuffer();
		sbSameRevGrpObj.append("select latest_obj.ID ");
		sbSameRevGrpObj.append("  from EIMVER obj_revision_group, ");
		sbSameRevGrpObj.append("       EIMVER same_revision_group, ");
		sbSameRevGrpObj.append("       EIMOBJ latest_obj ");
		sbSameRevGrpObj.append(" where obj_revision_group.VID = same_revision_group.VID ");
		sbSameRevGrpObj.append("   and obj_revision_group.OID in ( ");
		sbSameRevGrpObj.append(joinId(documentObjects, ","));
		sbSameRevGrpObj.append("       )");
		sbSameRevGrpObj.append("   and same_revision_group.OID = latest_obj.ID ");
		sbSameRevGrpObj.append("   and latest_obj.LATEST = 1 ");
		searchConditionGroup.addCondition(new SearchConditionIn(
				helper.opAnd(), PsedoAttributeTypeEnum.ID, SearchOperatorEnum.IN, sbSameRevGrpObj.toString()));

		// 返却項目設定
		List<AttributeTypeDomain> resultAttributes = new ArrayList<AttributeTypeDomain>();
		resultAttributes.add(PsedoAttributeTypeEnum.REV);
		resultAttributes.add(PsedoAttributeTypeEnum.REVISION_GROUP_ID);
		searchSelectObject.setResultAttrs(resultAttributes);

		// 検索実行
		List<ObjectDomain> latestObjectList = getObjectService().getList(searchSelectObject, null);
		
		// リビジョングループIDとLATEST:1の関連ドキュメントリストのMapを作成
		// 改訂中の場合、同一リビジョングループにLATEST;1が2件存在する。
		Map<Long, List<ObjectDomain>> revGrpIdAndLatestRelDocObjectListMap = new HashMap<Long, List<ObjectDomain>>();
		for (ObjectDomain latestObject : latestObjectList) {
			List<ObjectDomain> list = revGrpIdAndLatestRelDocObjectListMap.get(latestObject.getRevisionGroupId());
			if (list == null) {
				list = new ArrayList<ObjectDomain>();
				revGrpIdAndLatestRelDocObjectListMap.put(latestObject.getRevisionGroupId(), list);
			}
			list.add(latestObject);
		}
		
		Set<Long> idSet = new HashSet<Long>();
		for (ObjectDomain documentObject : documentObjects) {
			List<ObjectDomain> list = revGrpIdAndLatestRelDocObjectListMap.get(documentObject.getRevisionGroupId());
			if (list == null) {
				// ありえない
				continue;
			} else if (list.size() == 1) {
				// 出力してよい
				idSet.add(documentObject.getId());
			} else if (list.size() == 2) {
				// 改訂中の場合
				Boolean isLatestAndMaxRevision = false;
				for (ObjectDomain latest : list) {
					if (documentObject.getRevision() > latest.getRevision()) {
						isLatestAndMaxRevision = true;
					}
				}
				if (!isLatestAndMaxRevision) {
					// 最新でなければ出力してよい
					idSet.add(documentObject.getId());
				}
			}
		}
		return idSet;
	}

	/**
	 * ドキュメントオブジェクトの公開ファイルを設定します。
	 * 公開ファイルが存在しない場合は、nullを設定します。
	 * @param doc ドキュメントオブジェクト
	 * @since Ver6.6
	 */
	private void setPublicFileName(DocumentObjectDomain doc, Set<Long> publicDocumentIdSet) throws Exception{
		
		doc.setPublicFileName(null);

		if (doc.getStatus() == null) {
			// WFなし（即時公開）の場合

			// 公開ファイルの存在のチェック
			if (!publicDocumentIdSet.contains(doc.getId())) {
				return;
			}

			// 原本のファイル名を設定する
			doc.setPublicFileName(doc.getName());
			return;

		} else {
			// WF付の場合

			// 公開ファイルの存在のチェック
			FormatDomain publicFormat = getFormatService().getByDefinitionName(EIMConfig.get("FORMAT_NAME_PUBLIC"));
			FileDomain file = getFileDao().getByObjectAndFormat(doc, publicFormat);
			if (file == null) {
				return;
			}

			// 公開フォーマットのファイル名を設定する
			doc.setPublicFileName(file.getName());
		}
	}
	
	/**
	 * 関連ドキュメントの内、閲覧権限のないものを表示対象外にします。
	 * @param form フォーム
	 * @since Ver6.6
	 */
	private void filterReadAuthority(FormDomain form) throws Exception {

		// フォーム内の関連ドキュメント属性を取得
		List<AttributeDomain> relationDocumentAttributeList = getRelationDocumentAttributeList(form);

		// 関連ドキュメントのIDをリスト化
		MultipleCriteria<Long> docIds = new MultipleCriteria<Long>();
		for (AttributeDomain attribute : relationDocumentAttributeList) {

			// 関連ドキュメントの取得対象を設定
			for (ObjectDomain object : attribute.getObjectList()) {
				docIds.add(object.getId());
			}
		}
		if (docIds.size() == 0) {
			return;
		}

		// 関連ドキュメントの内、公開参照権限があるものを抽出
		ObjectCriteria objectCriteria = new ObjectCriteria();
		objectCriteria.setAccessRoleType(new AccessRoleTypeDomain(EIMAccessRole.READ));
		objectCriteria.setIds(docIds);
		objectCriteria.setAttributeTypeListForMultiTargetMethod(new ArrayList<AttributeTypeDomain>());
		objectCriteria.setItemListForMultiTargetMethod(new ArrayList<ObjectCriteria.ObjectItemEnum>());
		List<ObjectDomain> docObjs = getObjectService().getList(objectCriteria);

		// 読み取り権限のある関連ドキュメントのオブジェクトIDをリスト化
		Set<Long> visibleDocumentIdSet = new HashSet<Long>();
		for ( ObjectDomain object : docObjs) {
			visibleDocumentIdSet.add(object.getId());
		}

		// 除外対象属性リスト
		List<AttributeDomain> allInvisibleList = new ArrayList<AttributeDomain>();

		// 関連ドキュメント属性数ループ
		for (AttributeDomain attribute : relationDocumentAttributeList) {

			List<ObjectDomain> documentObjects = new ArrayList<ObjectDomain>();
			for ( ObjectDomain object : attribute.getObjectList()) {
				// 公開参照権限があるもののみを追加
				if (visibleDocumentIdSet.contains(object.getId())) {
					documentObjects.add(object);
				}
			}
			// 公開参照権限のあるドキュメントがある場合、属性を再設定する
			if (documentObjects.size() > 0){
				attribute.getObjectList().clear();
				attribute.getObjectList().addAll(documentObjects);

			// 公開参照権限のあるドキュメントが一つもない場合、除外対象属性に追加
			} else {
				allInvisibleList.add(attribute);
			}
		}
		// 権限により、要素数が０になった属性を除去する
		if (allInvisibleList.size() > 0) {
			form.getAttributeList().removeAll(allInvisibleList);
		}
	}
	
	/**
	 * 関連ドキュメントの内、公開済参照権限しかないドキュメントのIDセットを返します。
	 * @param attribute 関連ドキュメント属性
	 * @since Ver6.6
	 */
	private Set<Long> getAuthOnlyPublicReadSet(List<AttributeDomain> relationDocumentAttributeList) throws Exception {

		Set<Long> authOnlyPublicReadIdSet = new HashSet<Long>();

		// 関連ドキュメントのオブジェクトIDを設定
		MultipleCriteria<Long> docIds = new MultipleCriteria<Long>();
		for (AttributeDomain attribute : relationDocumentAttributeList) {
			
			for (ObjectDomain object : attribute.getObjectList()) {
				docIds.add(object.getId());
				authOnlyPublicReadIdSet.add(object.getId());
			}
		}
		// 関連ドキュメントがある場合、検索実行
		if (docIds.size() > 0) {
			
			// 関連ドキュメントの内、常時参照権限があるものを抽出
			ObjectCriteria objectCriteria = new ObjectCriteria();
			objectCriteria.setAccessRoleType(new AccessRoleTypeDomain(AppConstant.ACCESS_ROLE_ALWAYS_READ));
			objectCriteria.setIds(docIds);
			objectCriteria.setAttributeTypeListForMultiTargetMethod(new ArrayList<AttributeTypeDomain>());
			objectCriteria.setItemListForMultiTargetMethod(new ArrayList<ObjectCriteria.ObjectItemEnum>());
			List<ObjectDomain> docObjs = getObjectService().getList(objectCriteria);

			// 常時参照権限のある関連ドキュメントのオブジェクトIDを除去
			for (ObjectDomain object : docObjs) {
				authOnlyPublicReadIdSet.remove(object.getId());
			}
		}
		return authOnlyPublicReadIdSet;
	}
	
	//==================================
	// Getter/Setter
	//==================================
	/**
	 * リレーションタイプサービスを取得します。
	 * @return リレーションタイプサービス
	 */
	public RelationTypeService getRelationTypeService() {
		return relationTypeService;
	}

	/**
	 * リレーションタイプサービスを設定します。
	 * @param relationTypeService リレーションタイプサービス
	 */
	public void setRelationTypeService(RelationTypeService relationTypeService) {
		this.relationTypeService = relationTypeService;
	}
	
	/**
	 * リレーションサービスを取得します。
	 * @return リレーションサービス
	 */
	public RelationService getRelationService() {
		return relationService;
	}

	/**
	 * リレーションサービスを設定します。
	 * @param relationService リレーションサービス
	 */
	public void setRelationService(RelationService relationService) {
		this.relationService = relationService;
	}
	
	/**
	 * アクセス履歴サービスを取得します。
	 * @return アクセス履歴サービス
	 */
	public AccessHistoryService getAccessHistoryService() {
		return accessHistoryService;
	}

	/**
	 * アクセス履歴サービスを設定します。
	 * @param accessHistoryService アクセス履歴サービス
	 */
	public void setAccessHistoryService(AccessHistoryService accessHistoryService) {
		this.accessHistoryService = accessHistoryService;
	}

	/**
	 * オブジェクトサービス (取得項目なし)を取得します。
	 * @return オブジェクトサービス (取得項目なし)
	 */
	public ObjectService getObjectServiceWithoutAttribute() {
		return objectServiceWithoutAttribute;
	}

	/**
	 * オブジェクトサービス (取得項目なし)を設定します。
	 * @param objectServiceWithoutAttribute オブジェクトサービス (取得項目なし)
	 */
	public void setObjectServiceWithoutAttribute(ObjectService objectServiceWithoutAttribute) {
		this.objectServiceWithoutAttribute = objectServiceWithoutAttribute;
	}
}
