package common.enumeration;

/**
 * データ型・多重度で選択可能なUIコントロールマスタの情報を扱う列挙型クラス
 * 
 * @since Ver6.0
 */
public enum UIControlTypeByValueTypeAndMultipleEnum {
	
	/** 数値型・多重度なし */
	LONG_SINGLE		(new String[]{"TEXTINPUT"}),
	/** 数値型・多重度あり */
	LONG_MULTIPLE	(new String[]{"TEXTINPUT"}),
	
	/** 文字列型・多重度なし */
	STRING_SINGLE	(new String[]{"TEXTINPUT","TEXTAREA"}),
	/** 文字列型・多重度あり */
	STRING_MULTIPLE	(new String[]{"TEXTINPUT","TEXTAREA"}),
	
	/** 日付型・多重度なし */
	DATE_SINGLE		(new String[]{"CALENDER"}),
	/** 日付型・多重度あり */
	DATE_MULTIPLE	(new String[]{"CALENDER"}),
	
	/** テキスト型・多重度なし */
	TEXT_SINGLE		(new String[]{"TEXTAREA", "RICHTEXT"}),
	/** テキスト型・多重度あり */
	TEXT_MULTIPLE	(new String[]{"TEXTAREA", "RICHTEXT"}),
	
	/** 実数型・多重度なし */
	DOUBLE_SINGLE	(new String[]{"TEXTINPUT"}),
	/** 実数型・多重度あり */
	DOUBLE_MULTIPLE	(new String[]{"TEXTINPUT"}),
	
	/** オブジェクト型・多重度なし */
	OBJECT_SINGLE	(new String[]{"OBJECTSEARCH"}),
	/** オブジェクト型・多重度あり */
	OBJECT_MULTIPLE	(new String[]{"FILE", "OBJECTSEARCH"}),
	
	/** ユーザ型・多重度なし */
	USER_SINGLE		(new String[]{"USERSEARCH"}),
	/** ユーザ型・多重度あり */
	USER_MULTIPLE	(new String[]{"USERSEARCH"}),
	
	/** コード型・多重度なし */
	CODE_SINGLE		(new String[]{"COMBOBOX", "RADIOBUTTON"}),
	/** コード型・多重度あり */
	CODE_MULTIPLE	(new String[]{"COMBOBOX", "CHECKBOX"});


	/** データ型・多重度で選択可能なUIコントロールタイプ名称の配列 */
	private final String[] uiControlTypeNameList;

	private UIControlTypeByValueTypeAndMultipleEnum(String[] uiControlTypeNameList) {
		this.uiControlTypeNameList = uiControlTypeNameList;
	}

	public String[] getUiControlTypeNameList() {
		return uiControlTypeNameList;
	}
}