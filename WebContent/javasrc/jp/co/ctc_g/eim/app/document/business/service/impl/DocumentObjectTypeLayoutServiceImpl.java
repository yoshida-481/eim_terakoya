/**
 * 
 */
package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import common.bo.AttributeValueMaster;
import common.util.AttributeMasterUtil;
import eim.bo.EIMAttributeType;
import eim.bo.EIMValueType;
import eim.net.EIMSession;
import eim.util.DateUtils;
import eim.util.StringUtils;
import jp.co.ctc_g.eim.app.document.business.domain.DocumentAttributeTypeLayoutDomain;
import jp.co.ctc_g.eim.app.document.business.service.DocumentObjectTypeLayoutService;
import jp.co.ctc_g.eim.app.form.business.domain.AttributeTypeLayoutDomain;
import jp.co.ctc_g.eim.app.form.business.domain.ObjectTypeLayoutDomain;
import jp.co.ctc_g.eim.app.form.business.service.impl.ObjectTypeLayoutServiceImpl;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.CodeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.CodeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.OtherNameDomain;
import jp.co.ctc_g.eim.framework2.common.enumeration.ValueTypeEnum;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.common.util.ResourceUtils;

/**
 * オブジェクトタイプレイアウトサービスのドキュメント管理用拡張クラスです。以下の機能を拡張します。
 * <ul>
 * <li>値リストを取得し、コード型の属性としてレイアウト情報を返却します。
 * <li>リビジョンアップ時の引継ぎ属性タイプ、リビジョンアップ時に関連先を最新にするか否かの設定を返却します。
 * </ul>
 */
public class DocumentObjectTypeLayoutServiceImpl extends ObjectTypeLayoutServiceImpl implements DocumentObjectTypeLayoutService {
	
	/** 値リスト処理フラグ */
	private boolean handlingVlueList = false;
	
	/**
	 * オブジェクトタイプを登録します。
	 * その際、リビジョンアップ引継ぎフラグ、最新リビジョン関連付けフラグ属性を設定します。
	 * @see jp.co.ctc_g.eim.app.form.business.service.ObjectTypeLayoutService#create(jp.co.ctc_g.eim.app.form.business.domain.ObjectTypeLayoutDomain)
	 */
	public ObjectTypeLayoutDomain create(ObjectTypeLayoutDomain objectTypeLayout) throws Exception {
		
		// スーパークラスを呼び出す
		ObjectTypeLayoutDomain returnObjectTypeLayout = super.create(objectTypeLayout);

		if (returnObjectTypeLayout.getAttributeLayoutList() != null && !returnObjectTypeLayout.getAttributeLayoutList().isEmpty()) {
			// 引継ぎ・関連付け設定情報更新
			inheritanceAndRelationAttributeType(returnObjectTypeLayout);
		}

		return returnObjectTypeLayout;
	}

	/**
	 * オブジェクトタイプIDをキーにObjectTypeLayoutDomainを取得します。<br>
	 * 値リスト処理フラグがtrueの場合、値リストに対応します。値リストを取得し、コード型の属性としてレイアウト情報を返却します。<br>
	 * リビジョンアップ時の引継ぎ属性タイプ、リビジョンアップ時に関連先を最新にするか否かの設定を返却します。
	 * @see jp.co.ctc_g.eim.app.form.business.service.impl.ObjectTypeLayoutServiceImpl#getById(long)
	 */
	public ObjectTypeLayoutDomain getById(long id) throws Exception {
		
		// スーパークラスを呼び出す
		ObjectTypeLayoutDomain returnObjectTypeLayout = super.getById(id);
		
		// AttributeTypeLayoutDomainのListを取得しDocumentAttributeTypeLayoutDomainに変換する
		List<AttributeTypeLayoutDomain> attributeTypeLayoutDomainList = returnObjectTypeLayout.getAttributeLayoutList();
		List<AttributeTypeLayoutDomain> documentAttributeTypeLayoutDomainList = new ArrayList<AttributeTypeLayoutDomain>();
		for (AttributeTypeLayoutDomain attributeTypeLayoutDomain : attributeTypeLayoutDomainList) {
			documentAttributeTypeLayoutDomainList.add(new DocumentAttributeTypeLayoutDomain(attributeTypeLayoutDomain));
		}
		returnObjectTypeLayout.setAttributeLayoutList(documentAttributeTypeLayoutDomainList);

		// フラグ属性を設定
		setFlagAttributeTypeLayoutDomainList(documentAttributeTypeLayoutDomainList, returnObjectTypeLayout.getId());
		
		// 値リスト処理フラグがfalseの場合、リターンする
		if (!handlingVlueList) {
			return returnObjectTypeLayout;
		}
		
		// 値リストを返却する
		
		// documentAttributeTypeLayoutDomainListをMapに格納する、AttributeTypeDomainをEIMValueTypeに変換する
		Map<Long, AttributeTypeLayoutDomain> attributeTypeLayoutDomainMap = new HashMap<Long, AttributeTypeLayoutDomain>();
		List<EIMAttributeType> attributeTypeList = new ArrayList<EIMAttributeType>();
		
		for (AttributeTypeLayoutDomain attributeTypeLayoutDomain : documentAttributeTypeLayoutDomainList) {
			// 属性タイプIDをキーにattributeTypeLayoutDomainをMapに格納する
			attributeTypeLayoutDomainMap.put(attributeTypeLayoutDomain.getId(), attributeTypeLayoutDomain);
			
			// AttributeTypeDomainをEIMValueTypeに変換する
			EIMAttributeType attributeType = new EIMAttributeType(attributeTypeLayoutDomain.getId(), attributeTypeLayoutDomain.getDefinitionName(), null);
			attributeTypeList.add(attributeType);
		}
		
		// EIMValueTypeのリストを引数に値リストの有無を取得する
		EIMSession session = EIMThreadContext.getEIMSession();
		@SuppressWarnings("unchecked")
		HashSet<Long> masterExistSet = AttributeMasterUtil.getMasterExistSetByAttTypeIds(session, attributeTypeList);
		
		// 言語IDを取得する
		String langId = (String)EIMThreadContext.getTransactionContext().getLangId();
		
		// 値リストが存在する場合、値リストを取得する
		for (Iterator<Long> i = masterExistSet.iterator(); i.hasNext();) {
			AttributeValueMaster attributeValueMaster = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(session, i.next());
			
			// attributeTypeLayoutDomainをコード型の属性タイプとして編集する
			AttributeTypeLayoutDomain attributeTypeLayoutDomain = attributeTypeLayoutDomainMap.get(new Long(attributeValueMaster.getType().getId()));
			
			// バリュータイプをコード型にする
			attributeTypeLayoutDomain.setValueType(ValueTypeEnum.CODE);
			
			// UIControlをコンボボックスにするする
			attributeTypeLayoutDomain.setUiControlId("uIControlComboBox");
			
			List<String> attributeValueList = new ArrayList<String>();
			
			try {
				switch (attributeValueMaster.getType().getValueType().getId()) {
					case EIMValueType.INTEGER:
						for (long intValue : attributeValueMaster.getInts()) {
							attributeValueList.add(String.valueOf(intValue));
						}
						break;
					case EIMValueType.STRING:
						for (String stringValue : attributeValueMaster.getStrings()) {
							attributeValueList.add(stringValue);
						}
						break;
					case EIMValueType.DATE:
						for (Date dateValue : attributeValueMaster.getDates()) {
							// Date(TZ変換)→文字列変換
							attributeValueList.add(StringUtils.getDateStringByFormat(new Date(DateUtils.convDBTzToCLTzTime(session, dateValue)), ResourceUtils.getByKey("EIM.FORMAT.DATE")));
						}
						break;
					case EIMValueType.TEXT:
						for (String textValue : attributeValueMaster.getTexts()) {
							attributeValueList.add(textValue);
						}
						break;
					case EIMValueType.DOUBLE:
						for (double doubleValue : attributeValueMaster.getDoubles()) {
							attributeValueList.add(String.valueOf(doubleValue));
						}
						break;
				}
			}
			catch (EIMException eime)
			{
				// 値リストが設定値が存在しない場合
				if (eime.getMessageKey().equals("EIM.ERROR.COMMON.API.NO.ATTRIBUTE.MASTER.VALUE")) {
					// 無視する
				}
				else {
					throw eime;
				}
			}
			
			// コードタイプに値リストを設定する
			CodeTypeDomain codeTypeDomain = new CodeTypeDomain();
			long codeId = 0;
			for (String attributeValue : attributeValueList) {
				CodeDomain codeDomain = new CodeDomain();
				
				// ID
				codeDomain.setId(codeId);
				
				// コード値
				codeDomain.setCode(attributeValue);
				
				// 名前リスト
				OtherNameDomain otherNameDomain = new OtherNameDomain();
				otherNameDomain.setLangId(langId);
				otherNameDomain.setName(attributeValue);
				codeDomain.getNameList().add(otherNameDomain);
				
				codeTypeDomain.getCodeList().add(codeDomain);
				
				codeId ++;
			}
			attributeTypeLayoutDomain.setCodeType(codeTypeDomain);
		}
		
		return returnObjectTypeLayout;
	}

	/**
	 * @see jp.co.ctc_g.eim.app.form.business.service.ObjectTypeLayoutService#inheritanceAndRelationAttributeType(jp.co.ctc_g.eim.app.form.business.domain.ObjectTypeLayoutDomain)
	 * @since Ver6.6
	 */
	public void inheritanceAndRelationAttributeType(ObjectTypeLayoutDomain objectTypeLayout) throws Exception {
		if (objectTypeLayout == null) {
			throw new EIMException("EIM.ERROR.LOGIC.OBJECTTYPELAYOUT.VALUE.ILLEGAL");
		}

		// オブジェクトタイプオブジェクト取得
		ObjectTypeDomain objectType = new ObjectTypeDomain(ConfigUtils.getByKey("OBJECT_TYPE_NAME_OBJECTTYPE"));
		ObjectDomain objectTypeObject = getObjectService().getByTypeAndName(objectType, String.valueOf(objectTypeLayout.getId()));
		if (objectTypeObject == null) {
			throw new EIMException("EIM.ERROR.LOGIC.OBJECTTYPELAYOUT.NOTFOUND");
		}
		
		//
		// リビジョンアップ引継ぎフラグ設定
		//
		
		// リビジョンアップ引継ぎフラグがONの属性タイプIDをリストにつめる
		List<Long> inheritanceAttributeTypeIdList = new ArrayList<Long>();
		for(int i=0 ; i < objectTypeLayout.getAttributeLayoutList().size() ; i++){
			DocumentAttributeTypeLayoutDomain inheritanceAttributeType 
					= (DocumentAttributeTypeLayoutDomain)objectTypeLayout.getAttributeLayoutList().get(i);
			if(inheritanceAttributeType.isInheritanceFlag() == true){
				long id = objectTypeLayout.getAttributeLayoutList().get(i).getId();
				inheritanceAttributeTypeIdList.add(id);
			}
		}
		
		// リビジョンアップ引継ぎ属性取得
		AttributeDomain inheritanceAttribute = objectTypeObject.getAttribute(ConfigUtils.getByKey("ATTRIBUTE_TYPE_NAME_INHERITANCE"));
		
		// オブジェクトタイプオブジェクトにリビジョンアップ引継ぎフラグ情報設定
		if (inheritanceAttribute == null) {
			inheritanceAttribute = new AttributeDomain();
			objectTypeObject.getAttributeList().add(inheritanceAttribute);
			inheritanceAttribute.setAttributeType(new AttributeTypeDomain(ConfigUtils.getByKey("ATTRIBUTE_TYPE_NAME_INHERITANCE")));
		}
		
		// リビジョンアップ引継ぎ属性のValueにセット
		inheritanceAttribute.setLongList(inheritanceAttributeTypeIdList);
		
		//
		// 最新リビジョン関連付けフラグ設定
		//
		
		// 最新リビジョン関連付けフラグがONの属性タイプIDをリストにつめる
		List<Long> relationAttributeTypeIdList = new ArrayList<Long>();
		for(int i=0 ; i < objectTypeLayout.getAttributeLayoutList().size() ; i++){
			DocumentAttributeTypeLayoutDomain relationAttributeType 
				= (DocumentAttributeTypeLayoutDomain)objectTypeLayout.getAttributeLayoutList().get(i);
			if(relationAttributeType.isRelationFlag() == true){
				long id = objectTypeLayout.getAttributeLayoutList().get(i).getId();
				relationAttributeTypeIdList.add(id);
			}
		}
		
		// 最新リビジョン関連付け属性取得
		AttributeDomain relationAttribute = objectTypeObject.getAttribute(ConfigUtils.getByKey("ATTRIBUTE_TYPE_NAME_RELATION"));
		
		// オブジェクトタイプオブジェクトに最新リビジョン関連付けフラグ情報設定
		if (relationAttribute == null) {
			relationAttribute = new AttributeDomain();
			objectTypeObject.getAttributeList().add(relationAttribute);
			relationAttribute.setAttributeType(new AttributeTypeDomain(ConfigUtils.getByKey("ATTRIBUTE_TYPE_NAME_RELATION")));
		}
		
		// 最新リビジョン関連付け属性のValueにセット
		relationAttribute.setLongList(relationAttributeTypeIdList);
		
		
		getObjectService().update(objectTypeObject);
		
	}
	
	//
	// Privateメソッド
	//
	
	/**
	 * AttributeTypeLayoutDomainの各フラグを設定します。<br>
	 * 指定したオブジェクトタイプに紐付く、「オブジェクトタイプ」オブジェクトを取得します。<br>
	 * 「オブジェクトタイプ」オブジェクトの各フラグ属性に設定された設定対象の属性タイプIDリストに対応するAttributeTypeLayoutDomainに対して、各フラグを設定します。<br>
	 * @param  attributeTypeLayoutList 設定対象のAttributeTypeLayoutDomainリスト
	 * @param  objectTypeId オブジェクトタイプID
	 * @since Ver6.6
	 */
	private void setFlagAttributeTypeLayoutDomainList(List<AttributeTypeLayoutDomain> attributeTypeLayoutList, long objectTypeId) throws Exception {
		
		Set<Long> inheritanceAttributeTypeIdSet = new HashSet<Long>();
		Set<Long> relationAttributeTypeIdSet = new HashSet<Long>();
		
		// オブジェクトタイプオブジェクト取得
		ObjectTypeDomain objectType = new ObjectTypeDomain(ConfigUtils.getByKey("OBJECT_TYPE_NAME_OBJECTTYPE"));
		ObjectDomain objectTypeObject = getObjectService().getByTypeAndName(objectType, String.valueOf(objectTypeId));
		if (objectTypeObject != null) {

			inheritanceAttributeTypeIdSet = createUpdateFlagTargetAttributeTypeIdSet(objectTypeObject, "ATTRIBUTE_TYPE_NAME_INHERITANCE");
			relationAttributeTypeIdSet = createUpdateFlagTargetAttributeTypeIdSet(objectTypeObject, "ATTRIBUTE_TYPE_NAME_RELATION");
		}

		for(int i = 0; i < attributeTypeLayoutList.size(); i++){
			AttributeTypeLayoutDomain attributeTypeLayout = attributeTypeLayoutList.get(i);
			setInheritanceFlag((DocumentAttributeTypeLayoutDomain)attributeTypeLayout, inheritanceAttributeTypeIdSet);
			setRelationFlag((DocumentAttributeTypeLayoutDomain)attributeTypeLayout, relationAttributeTypeIdSet);
		}
		
		return;
		
	}

	/**
	 * オブジェクトタイプオブジェクトの属性値を元に、フラグ設定対象の属性タイプIDのセットを作成します。
	 * @param  objectTypeObject オブジェクトタイプオブジェクトドメイン
	 * @param  attributeDefinitionName フラグが設定された属性の定義名称
	 * @since Ver6.6
	 */
	private Set<Long> createUpdateFlagTargetAttributeTypeIdSet(ObjectDomain objectTypeObject, String attributeDefinitionName) throws Exception {
		
		Set<Long> attributeTypeIdSet = new HashSet<Long>();
		
		AttributeDomain attribute = objectTypeObject.getAttribute(ConfigUtils.getByKey(attributeDefinitionName));
		if (attribute == null) {
			// 対象属性が存在しない場合空を返却する(変更対象なしの扱いとする)
			return attributeTypeIdSet;
		}
		for (int i=0; i< attribute.getLongList().size(); i++) {
			attributeTypeIdSet.add(attribute.getLongList().get(i));
		}
		
		return attributeTypeIdSet;
	}
	
	/**
	 * 対象のAttributeTypeLayoutDomainのリビジョンアップ引継ぎフラグを設定します。<br>
	 * @param  attributeTypeLayout リビジョンアップ引継ぎフラグ設定対象のAttributeTypeLayoutDomain
	 * @param  attributeTypeIdSet 引継ぎ対象の属性IDが格納されているSet
	 * @since Ver6.6
	 */
	private void setInheritanceFlag(DocumentAttributeTypeLayoutDomain attributeTypeLayout, Set<Long> attributeTypeIdSet) throws Exception {
			attributeTypeLayout.setInheritanceFlag(
					attributeTypeIdSet.contains(attributeTypeLayout.getId()));
	}

	/**
	 * 対象のAttributeTypeLayoutDomainの最新リビジョン関連付けフラグを設定します。<br>
	 * @param  attributeTypeLayout 最新リビジョン関連付けフラグ設定対象のAttributeTypeLayoutDomain
	 * @param  attributeTypeIdSet 関連付け対象の属性IDが格納されているSet
	 * @since Ver6.6
	 */
	private void setRelationFlag(DocumentAttributeTypeLayoutDomain attributeTypeLayout, Set<Long> attributeTypeIdSet) throws Exception {
			attributeTypeLayout.setRelationFlag(
					attributeTypeIdSet.contains(attributeTypeLayout.getId()));
	}
	
	//
	// Getter/Setter
	//
	
	/**
	 * 値リスト処理フラグを返却します。
	 * trueの場合、変換を行います。デフォルトはfalseです。
	 * @return handlingVlueList
	 */
	public boolean isHandlingVlueList() {
		return handlingVlueList;
	}

	/**
	 * 値リスト処理フラグを設定します。
	 * trueの場合、変換を行います。デフォルトはfalseです。
	 * @param handlingVlueList
	 */
	public void setHandlingVlueList(boolean handlingVlueList) {
		this.handlingVlueList = handlingVlueList;
	}


}
