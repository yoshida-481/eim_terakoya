package jp.co.ctc_g.eim.app.document.business.domain;

import jp.co.ctc_g.eim.app.form.business.domain.AttributeTypeLayoutDomain;

/**
 * ドキュメント用属性タイプのレイアウト情報を保持します.
 * @since Ver 6.6
 */
public class DocumentAttributeTypeLayoutDomain extends AttributeTypeLayoutDomain {

	/** リビジョンアップ引継ぎフラグ(true:引継ぎ対象) */
	private boolean inheritanceFlag = false;
	
	/** 最新リビジョン関連付けフラグ(true:関連付け対象) */
	private boolean relationFlag = false;
	
	/** 下位引継ぎ属性フラグ(true:引継ぎ属性) */
	private boolean successionFlag = false;
	
	/**
	 * コンストラクタ<br>
	 * プロパティ値は全てデフォルト値となります。
	 * @since Ver6.6
	 */
	public DocumentAttributeTypeLayoutDomain() {
		super();
	}

	/**
	 * コンストラクタ<br>
	 * 属性タイプIDを設定します。
	 * @param id 属性タイプID
	 * @since Ver6.6
	 */
	public DocumentAttributeTypeLayoutDomain(long id) {
	    super(id);
	}

	/**
	 * コンストラクタ<br>
	 * 属性タイプ定義名称を設定します。
	 * @param definitionName 属性タイプ定義名称
	 * @since Ver6.6
	 */
	public DocumentAttributeTypeLayoutDomain(String definitionName) {
	    super(definitionName);
	}

	/**
	 * コンストラクタ<br>
	 * 属性タイプのフィールド値を設定します。
	 * @param attributeTypeLayout 属性タイプ
	 * @since Ver6.6
	 */
	public DocumentAttributeTypeLayoutDomain(AttributeTypeLayoutDomain attributeTypeLayout) {

		// スーパークラスを呼び出す
		super(attributeTypeLayout);
		
		if (attributeTypeLayout != null) {
			setVisible(attributeTypeLayout.isVisible());
			setRequired(attributeTypeLayout.isRequired());
			setUiControlType(attributeTypeLayout.getUiControlType());
			setUiControlId(attributeTypeLayout.getUiControlId());
			setSearchMasterDisplayConfig(attributeTypeLayout.getSearchMasterDisplayConfig());
			setOrderSetFlag(attributeTypeLayout.isOrderSetFlag());
			setNewCopyFlag(attributeTypeLayout.getNewCopyFlag());
			setFormListColumnId(attributeTypeLayout.getFormListColumnId());
			setInitialLongValueList(attributeTypeLayout.getInitialLongValueList());
			setInitialStringValueList(attributeTypeLayout.getInitialStringValueList());
			setInitialDoubleValueList(attributeTypeLayout.getInitialDoubleValueList());
			setInitialCodeValueList(attributeTypeLayout.getInitialCodeValueList());
			setInitialUserValueList(attributeTypeLayout.getInitialUserValueList());
		}
	}

	/**
	 * リビジョンアップ引継ぎフラグ(true:引継ぎ対象)を取得します。
	 * @return リビジョンアップ引継ぎフラグ(true:引継ぎ対象)
	 * @since Ver6.6
	 */
	public boolean isInheritanceFlag() {
		return inheritanceFlag;
	}

	/**
	 * リビジョンアップ引継ぎフラグ(true:引継ぎ対象)を設定します。
	 * @param inheritanceFlag リビジョンアップ引継ぎフラグ(true:引継ぎ対象)
	 * @since Ver6.6
	 */
	public void setInheritanceFlag(boolean inheritanceFlag) {
		this.inheritanceFlag = inheritanceFlag;
	}

	/**
	 * 最新リビジョン関連付けフラグ(true:関連付け対象)を取得します。
	 * @return 最新リビジョン関連付けフラグ(true:関連付け対象)
	 * @since Ver6.6
	 */
	public boolean isRelationFlag() {
		return relationFlag;
	}

	/**
	 * 最新リビジョン関連付けフラグ(true:関連付け対象)を設定します。
	 * @param relationFlag 最新リビジョン関連付けフラグ(true:関連付け対象)
	 * @since Ver6.6
	 */
	public void setRelationFlag(boolean relationFlag) {
		this.relationFlag = relationFlag;
	}
	
	/**
	 * 下位引継ぎ属性フラグ(true:引継ぎ属性)を取得します。
	 * @return 下位引継ぎ属性フラグ(true:引継ぎ属性)
	 * @since Ver6.6
	 */
	public boolean isSuccessionFlag() {
		return successionFlag;
	}

	/**
	 * 下位引継ぎ属性フラグ(true:引継ぎ属性)を設定します。
	 * @param successionFlag 下位引継ぎ属性フラグ(true:引継ぎ属性)
	 * @since Ver6.6
	 */
	public void setSuccessionFlag(boolean successionFlag) {
		this.successionFlag = successionFlag;
	}

}
