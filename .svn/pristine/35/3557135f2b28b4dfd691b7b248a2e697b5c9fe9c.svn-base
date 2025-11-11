package jp.co.ctc_g.eim.common.presentation.web.dto;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.springframework.context.ApplicationContext;

import common.util.AppConstant;
import common.util.AppValueTypeUtil;
import common.util.NamespaceUtil;
import eim.bo.EIMResource;
import eim.net.EIMSession;
import eim.util.StringUtils;
import jp.co.ctc_g.eim.app.form.business.domain.AttributeTypeLayoutDomain;
import jp.co.ctc_g.eim.app.form.business.domain.UIControlConfDomain;
import jp.co.ctc_g.eim.app.form.business.service.UIControlConfService;
import jp.co.ctc_g.eim.common.presentation.web.dto.DefValueDTO.DefValueList;
import jp.co.ctc_g.eim.common.presentation.web.dto.InitValueDTO.InitValueList;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusTypeDomain;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.domain.entity.CodeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.CodeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.OtherNameDomain;
/**
 * AttTypesDTO
 * <p>
 * Struts->SpringMVCへの移行に際して、クライアントへ送信するXMLデータを再現したデータ転送用オブジェクトです。
 */
public class AttTypesDTO {

	

	/**
	 * Strutsで生成したXMLデータにおける配列要素を再現するためのクラスです。
	 */
	public static class AttTypes {
		/** 属性リスト */
		private List<AttTypesDTO> attType = new ArrayList<AttTypesDTO>();

		/**
		 * コンストラクタ
		 * 
		 * @param attrTypeLayoutList 
		 * @param statusType
		 * @throws Exception 
		 */
		public AttTypes(List<AttributeTypeLayoutDomain> attrTypeLayoutList, StatusTypeDomain statusType) throws Exception {
			// 帳票管理システムの並び順用
			int dispOrder = 0;
			
			for (AttributeTypeLayoutDomain domain : attrTypeLayoutList) {
				dispOrder ++;
				attType.add(new AttTypesDTO(domain, dispOrder));
			}
		}

		public List<AttTypesDTO> getAttType() {
			return attType;
		}

		public void setAttType(List<AttTypesDTO> attType) {
			this.attType = attType;
		}
	}

	/**
	 * クライアントへ送信していたXMLデータにおいて、単数値のプロパティを保持する親要素を再現するためのクラスです。
	 */
	public class Attr {

		private String attTypeId = null;
		private String attTypeName = null;
		private String definitionName = null;
		private String valTypeId = null;
		private String valTypeName = null;
		private String isMultipleValue = null;
		private String codeTypeName = null;
		private String attTypeEssential = null;
		private String uiControlName = null;
		private String dispOrder = null;
		private String newCopyFlag = null;

		/**
		 * コンストラクタ
		 * 
		 * @param attTypeLayoutDomain 
		 * @param dispOrderStr
		 * @throws Exception 
		 */
		private Attr(AttributeTypeLayoutDomain attTypeLayoutDomain) throws Exception {

			setAttTypeId(String.valueOf(attTypeLayoutDomain.getId()));
			setAttTypeName(attTypeLayoutDomain.getName());
			
			EIMSession sess = EIMThreadContext.getEIMSession();
			String adminAppId = (String)sess.getAttribute("ADMIN_APP_ID");


			setValTypeId(String.valueOf(attTypeLayoutDomain.getValueType().getValue()));
			setValTypeName(AppValueTypeUtil.getValueTypeName(sess, attTypeLayoutDomain.getValueType().getValue()));
			
			// 複数値属性タイプの場合
			if (attTypeLayoutDomain.isMultiple()) {
				setIsMultipleValue(EIMResource.getMessage(sess, "EIM.LABEL.ATTRIBUTE.MULTIPLE.MAINTENANCE"));	// 保持する
			} else {
				setIsMultipleValue(EIMResource.getMessage(sess, "EIM.LABEL.ATTRIBUTE.MULTIPLE.NONE"));		// なし
			}
			setIsMultipleValue(isMultipleValue);

			if (attTypeLayoutDomain.getCodeType() != null) {
				setCodeTypeName(attTypeLayoutDomain.getCodeType().getDefinitionName());
			}
	
			// 帳票管理の場合
			if (adminAppId.equals(AppConstant.ADMIN_APP_ID_FORM)) {
				setAttTypeEssential(String.valueOf(attTypeLayoutDomain.isRequired()));
				
				// UIコントロール名称のマッピング情報
				Map<String, String> uiContorolId_name_map = new HashMap<String, String>();

				// 帳票管理の場合
				if (adminAppId.equals(AppConstant.ADMIN_APP_ID_FORM)) {
					
					// Serviceを取得
					ApplicationContext context = ApplicationContextLoader.getContext();

					// UIコントロールサービス
					UIControlConfService uIControlConfService = 
						(UIControlConfService)context.getBean("uIControlConfService");
					
					// UIコントロール定義リストを取得
					List<UIControlConfDomain> uiControlConfList = uIControlConfService.getList();
					
					// ログイン言語を取得
					String langId = (String) sess.getAttribute("langId");
					String name = "";
					// UIコントロール定義リストをマッピング
					for (UIControlConfDomain uiControlConf : uiControlConfList) {
						// ログイン言語のUIコントロール他言語名称を取得
						List<OtherNameDomain> otherNameList = uiControlConf.getNameList();
						for (OtherNameDomain otherName : otherNameList) {
							if (otherName.getLangId().equals(langId)) {
								name = otherName.getName();
								break;
							}
						}
						// マッピング
						uiContorolId_name_map.put(uiControlConf.getBeanId(), name);
					}
					
					String tmpUiControlId = attTypeLayoutDomain.getUiControlId();
					if( tmpUiControlId != null ){
						if (uiContorolId_name_map.containsKey(tmpUiControlId)) {
							
							setUiControlName(uiContorolId_name_map.get(tmpUiControlId));
						}
					}
				}

				// 複製情報が設定されている場合
				if (attTypeLayoutDomain.getNewCopyFlag() == true) {
					setNewCopyFlag("true");
				}else if(attTypeLayoutDomain.getNewCopyFlag() == false){
					setNewCopyFlag("false");
				}
				// ネームスペースを除いた定義名称
				setDefinitionName(NamespaceUtil.getDefNamenWhichExceptedNamespace(
						attTypeLayoutDomain.getDefinitionName()));	
			} else {
				// 汎用システム管理の場合
				// ネームスペース付き定義名称
				setDefinitionName(NamespaceUtil.getDefNameWithNamespaceParentheses(
						attTypeLayoutDomain.getName(), attTypeLayoutDomain.getDefinitionName()));
			}
				
		}

		public String getAttTypeId() {
			return attTypeId;
		}

		public void setAttTypeId(String attTypeId) {
			this.attTypeId = attTypeId;
		}

		public String getAttTypeName() {
			return attTypeName;
		}

		public void setAttTypeName(String attTypeName) {
			this.attTypeName = attTypeName;
		}

		public String getDefinitionName() {
			return definitionName;
		}

		public void setDefinitionName(String definitionName) {
			this.definitionName = definitionName;
		}

		public String getValTypeId() {
			return valTypeId;
		}

		public void setValTypeId(String valTypeId) {
			this.valTypeId = valTypeId;
		}

		public String getValTypeName() {
			return valTypeName;
		}

		public void setValTypeName(String valTypeName) {
			this.valTypeName = valTypeName;
		}

		public String getIsMultipleValue() {
			return isMultipleValue;
		}

		public void setIsMultipleValue(String isMultipleValue) {
			this.isMultipleValue = isMultipleValue;
		}

		public String getCodeTypeName() {
			return codeTypeName;
		}

		public void setCodeTypeName(String codeTypeName) {
			this.codeTypeName = codeTypeName;
		}

		public String getAttTypeEssential() {
			return attTypeEssential;
		}

		public void setAttTypeEssential(String attTypeEssential) {
			this.attTypeEssential = attTypeEssential;
		}

		public String getUiControlName() {
			return uiControlName;
		}

		public void setUiControlName(String uiControlName) {
			this.uiControlName = uiControlName;
		}

		public String getDispOrder() {
			return dispOrder;
		}

		public void setDispOrder(String dispOrder) {
			this.dispOrder = dispOrder;
		}

		public String getNewCopyFlag() {
			return newCopyFlag;
		}

		public void setNewCopyFlag(String newCopyFlag) {
			this.newCopyFlag = newCopyFlag;
		}

	}

	/**
	 * Strutsで生成したXMLデータにおいて単数値のプロパティを保持する親要素です。
	 */
	private Attr attr = null;

	private DefValueList defValueList = null;
	private InitValueList initValueList = null;
	
	/**
	 * コンストラクタ
	 *
	 * @param attrTypeLayoutDomain
	 * @param dispOrder
	 * @throws Exception
	 */
	public AttTypesDTO(AttributeTypeLayoutDomain attrTypeLayoutDomain, int dispOrder) throws Exception {

		// コードマップ（key=id, value=name）
		Map<Long, String> codeMap = new HashMap<Long, String>();
		
		// コードマップ作成済みコードタイプIDセット
		Set<Long> codeTypeIdSet = new HashSet<Long>();

		EIMSession sess = EIMThreadContext.getEIMSession();
		String adminAppId = (String)sess.getAttribute("ADMIN_APP_ID");
		
		// 属性タイプのデフォルト値リストを取得
		// 数値型
		List<Long> defaultLongValueList = attrTypeLayoutDomain.getDefaultLongValueList();
	
		// 文字列型
		List<String> defaultStringValueList = attrTypeLayoutDomain.getDefaultStringValueList();
		for(int i = 0; defaultStringValueList.size() > i ; i++) {
			defaultStringValueList.set(i, StringUtils.xmlEncode(defaultStringValueList.get(i)));
		}
	
		// テキスト型
		List<String> defaultTextValueList = attrTypeLayoutDomain.getDefaultTextValueList();
		for(int i = 0; defaultTextValueList.size() > i ; i++) {
			defaultTextValueList.set(i, StringUtils.xmlEncode(defaultTextValueList.get(i)));
		}
		
		// 日付型
		List<Date> defaultDateValueList = attrTypeLayoutDomain.getDefaultDateValueList();
		
		// 実数型
		List<Double> defaultDoubleValueList = attrTypeLayoutDomain.getDefaultDoubleValueList();
			
		// ---------------------------------------------------------------------------------------------------------------------------------------
		
		attr = new Attr(attrTypeLayoutDomain);
		
		defValueList = new DefValueList(defaultLongValueList,defaultStringValueList,defaultTextValueList,
				defaultDateValueList,defaultDoubleValueList);
		
		// 帳票管理用システム管理の場合
		if (adminAppId.equals(AppConstant.ADMIN_APP_ID_FORM)) {
			// 表示順が設定されている場合
			if (attrTypeLayoutDomain.isOrderSetFlag()) {
				attr.setDispOrder(String.valueOf(dispOrder));
			}
			
			// 属性タイプの初期値リストを取得
			// 数値型
			List<Long> initLongValueList = attrTypeLayoutDomain.getInitialLongValueList();

			// 文字列型
			List<String> initStringValueList = attrTypeLayoutDomain.getInitialStringValueList();
			for(int i = 0; initStringValueList.size() > i ; i++) {
				initStringValueList.set(i, StringUtils.xmlEncode(initStringValueList.get(i)));
			}
		
			// 実数型
			List<Double> initDoubleValueList = attrTypeLayoutDomain.getInitialDoubleValueList();
			
			// コード型
			List<String> initCodeValueList = new ArrayList<String>();
			CodeTypeDomain codeTypeDomain = attrTypeLayoutDomain.getCodeType();
			// コードマップ未作成のコードタイプの場合
			if(codeTypeDomain != null
					&& !codeTypeIdSet.contains(codeTypeDomain.getId()))
			{
				/* マッピング追加 */
				List<CodeDomain> codeDomainList = codeTypeDomain.getCodeList();
				for(CodeDomain codeDomain : codeDomainList)
				{
					codeMap.put(codeDomain.getId(), codeDomain.getName());
				}
			}
			
			for(Long initCodeValue : attrTypeLayoutDomain.getInitialCodeValueList()) {
				
				// IDをキーに名称を取得、出力
				String codeName = codeMap.get(initCodeValue);
				if(codeName != null){
					initCodeValueList.add(StringUtils.xmlEncode(codeName));
				}
			}

			initValueList = new InitValueList(initLongValueList,initStringValueList,
					initDoubleValueList,initCodeValueList);
		}
	}
	
	/**
	 * @return attrを取得します。
	 */
	public Attr getAttr() {
		return attr;
	}

	/**
	 * @param attrを設定します。
	 */
	public void setAttr(Attr attr) {
		this.attr = attr;
	}

	/**
	 * @return defValueListを取得します。
	 */
	public DefValueList getDefValueList() {
		return defValueList;
	}

	/**
	 * @param defValueListを設定します。
	 */
	public void setDefValueList(DefValueList defValueList) {
		this.defValueList = defValueList;
	}

	/**
	 * @return initValueListを取得します。
	 */
	public InitValueList getInitValueList() {
		return initValueList;
	}

	/**
	 * @param initValueListを設定します。
	 */
	public void setInitValueList(InitValueList initValueList) {
		this.initValueList = initValueList;
	}
}
