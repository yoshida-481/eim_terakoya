package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jp.co.ctc_g.eim.app.document.business.domain.DocumentAttributeTypeLayoutDomain;
import jp.co.ctc_g.eim.app.document.business.service.DocumentFormTypeService;
import jp.co.ctc_g.eim.app.form.business.domain.AttributeTypeLayoutDomain;
import jp.co.ctc_g.eim.app.form.business.domain.FormTypeDomain;
import jp.co.ctc_g.eim.app.form.business.service.impl.FormTypeServiceImpl;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.common.enumeration.ValueTypeEnum;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.common.util.ResourceUtils;
import eim.net.EIMSession;
import eim.util.DateUtils;
import eim.util.StringUtils;

public class DocumentFormTypeServiceImpl extends FormTypeServiceImpl implements DocumentFormTypeService {
	
	/** オブジェクトサービス */
	private ObjectService objectService;
	
	/**
	 * 帳票タイプを取得します。<br>
	 * 下位引継ぎ属性情報を付加します。<br>
	 * さらに、「改訂内容」は新規登録時に非表示となるためレイアウト情報から削除します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.DocumentFormTypeService#getByIdAndParent(long, long)
	 */
	public FormTypeDomain getByIdAndParent(long id, long parentId) throws Exception {
		
		// 文書タイプ取得
		FormTypeDomain formType = super.getById(id);
		// 親オブジェクト取得(ワークスペース、または、フォルダ)
		ObjectDomain parent = objectService.getById(parentId);
		
		// 下位引継ぎ属性設定
		if (parent != null)
			setSuccessionAttr(formType, parent);
		
		// 新規登録時の非表示項目(改訂内容)を除外する
		for (AttributeTypeLayoutDomain attributeTypeLayout : formType.getFormLayout().getObjectTypeLayout().getAttributeLayoutList()) {
			if (attributeTypeLayout.getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_REV_CONTENT"))) {
				formType.getFormLayout().getObjectTypeLayout().getAttributeLayoutList().remove(attributeTypeLayout);
				break;
			}
		}
		
		return formType;
	}
	
	/**
	 * 下位引継ぎ属性設定の処理
	 * @param formType 文書タイプ
	 * @param parent 親オブジェクト(ワークスペース、または、フォルダ)
	 */
	private void setSuccessionAttr(FormTypeDomain formType, ObjectDomain parent) throws Exception {
		
		// 下位引継ぎ対象の属性タイプIDリスト
		Set<Long> successionIdSet = new HashSet<Long>();
		AttributeDomain successionAttr = parent.getAttribute(ConfigUtils.getByKey("ATTR_NAME_FOLDER_TO_LOW_ATTR"));
		if(successionAttr != null) {
			successionIdSet = new HashSet<Long>(successionAttr.getLongList());
		}
		
		// 下位引継ぎ対象の属性マップ(KEY:属性タイプID、VALUE:属性)
		Map<Long, AttributeDomain> successionAttrMap = new HashMap<Long, AttributeDomain>();
		for(AttributeDomain attr: parent.getAttributeList()) {
			if(successionIdSet.contains(attr.getAttributeType().getId())) {
				successionAttrMap.put(attr.getAttributeType().getId(), attr);
			}
		}
		
		EIMSession session = EIMThreadContext.getEIMSession();
		
		// 属性タイプレイアウト情報に下位引継ぎ属性情報を設定
		for(AttributeTypeLayoutDomain attrLayout: formType.getFormLayout().getObjectTypeLayout().getAttributeLayoutList()) {
			if(!successionIdSet.contains(attrLayout.getId())) continue;
			
			// 下位引継ぎ属性フラグをON
			((DocumentAttributeTypeLayoutDomain)attrLayout).setSuccessionFlag(true);
			
			// 属性の初期値を設定
			AttributeDomain attr = successionAttrMap.get(attrLayout.getId());
			if(attr == null) continue;
			
			// 下位引継ぎ属性(親の属性)のデータ型に応じてUIコントロール、データ型、初期値を設定する (コード型に変換済みのものがあるため上書きする)
			switch(attr.getAttributeType().getValueType()) {
				case LONG:
					attrLayout.setUiControlId(ConfigUtils.getByKey("UI_CONTROL_ID_TEXT_INPUT"));
					attrLayout.setUiControlType(ConfigUtils.getByKey("UI_CONTROL_TYPE_TEXT_INPUT"));
					attrLayout.setValueType(ValueTypeEnum.LONG);
					
					attrLayout.getInitialLongValueList().clear();
					attrLayout.setInitialLongValueList(attr.getLongList());
					break;
				case STRING:
					attrLayout.setUiControlId(ConfigUtils.getByKey("UI_CONTROL_ID_TEXT_INPUT"));
					attrLayout.setUiControlType(ConfigUtils.getByKey("UI_CONTROL_TYPE_TEXT_INPUT"));
					attrLayout.setValueType(ValueTypeEnum.STRING);
					
					attrLayout.getInitialStringValueList().clear();
					attrLayout.setInitialStringValueList(attr.getStringList());
					break;
				case DATE:
					// 日付を文字列として表示
					List<String> dateStrList = new ArrayList<String>();
					for(Date date: attr.getDateList()) {
						Date convertedDateValue = new Date(DateUtils.convDBTzToCLTzTime(session, date));
						String stringValue = StringUtils.getDateStringByFormat(convertedDateValue, ResourceUtils.getByKey("EIM.FORMAT.DATE"));
						dateStrList.add(stringValue);
					}
					attrLayout.setUiControlId(ConfigUtils.getByKey("UI_CONTROL_ID_TEXT_INPUT"));
					attrLayout.setUiControlType(ConfigUtils.getByKey("UI_CONTROL_TYPE_TEXT_INPUT"));
					attrLayout.setValueType(ValueTypeEnum.STRING);
					attrLayout.getInitialStringValueList().clear();
					attrLayout.setInitialStringValueList(dateStrList);
					break;
				case TEXT:
					attrLayout.getInitialStringValueList().clear();
					attrLayout.setInitialStringValueList(attr.getTextList());
					break;
				case DOUBLE:
					attrLayout.setUiControlId(ConfigUtils.getByKey("UI_CONTROL_ID_TEXT_INPUT"));
					attrLayout.setUiControlType(ConfigUtils.getByKey("UI_CONTROL_TYPE_TEXT_INPUT"));
					attrLayout.setValueType(ValueTypeEnum.DOUBLE);
					
					attrLayout.getInitialDoubleValueList().clear();
					attrLayout.setInitialDoubleValueList(attr.getDoubleList());
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
//			}
		}
	}
	
	//==================================
	// Getter/Setter
	//==================================
	/**
	 * オブジェクトサービスを取得します。
	 * @return オブジェクトサービス
	 */
	public ObjectService getObjectService() {
		return objectService;
	}

	/**
	 * オブジェクトサービスを設定します。
	 * @param objectService オブジェクトサービス
	 */
	public void setObjectService(ObjectService objectService) {
		this.objectService = objectService;
	}
}
