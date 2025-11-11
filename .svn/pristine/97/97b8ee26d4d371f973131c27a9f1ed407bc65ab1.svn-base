package jp.co.ctc_g.eim.common.presentation.web.dto;

import java.util.ArrayList;
import java.util.List;

/**
 * InitValueDTO
 * <p>
 * Struts->SpringMVCへの移行に際して、クライアントへ送信するXMLデータを再現したデータ転送用オブジェクトです。
 */
public class InitValueDTO<T> {

	/**
	 * Strutsで生成したXMLデータにおける配列要素を再現するためのクラスです。
	 */
	public static class InitValueList {

		@SuppressWarnings("rawtypes")
		private List<InitValueDTO> initValue = new ArrayList<InitValueDTO>();

		/**
		 * コンストラクタ
		 * @param initLongValueList 
		 * @param initStringValueList 
		 * @param initDoubleValueList 
		 * @param initCodeValueList 
		 */
		@SuppressWarnings({ "unchecked", "rawtypes" })
		public InitValueList(List<Long> initLongValueList, List<String> initStringValueList,
				List<Double> initDoubleValueList, List<String> initCodeValueList) {
			for (Long value : initLongValueList) {
				initValue.add(new InitValueDTO(value));
			}
			for (String value : initStringValueList) {
				initValue.add(new InitValueDTO(value));
			}
			for (Double value : initDoubleValueList) {
				initValue.add(new InitValueDTO(value));
			}
			for (String value : initCodeValueList) {
				initValue.add(new InitValueDTO(value));
			}
		}

		@SuppressWarnings("rawtypes")
		public List<InitValueDTO> getInitValue() {
			return initValue;
		}

		@SuppressWarnings("rawtypes")
		public void setInitValue(List<InitValueDTO> initValue) {
			this.initValue = initValue;
		}

	}

	/**
	 * クライアントへ送信していたXMLデータにおいて、単数値のプロパティを保持する親要素を再現するためのクラスです。
	 */
	public class Attr {

		private String value = null;

		/**
		 * コンストラクタ
		 *
		 * @param val
		 */
		private Attr(T val) {
			setValue(String.valueOf(val));
		}

		public String getValue() {
			return value;
		}

		public void setValue(String value) {
			this.value = value;
		}

	}

	/**
	 * Strutsで生成したXMLデータにおいて単数値のプロパティを保持する親要素です。
	 */
	private Attr attr = null;

	/**
	 * コンストラクタ
	 *
	 * @param value
	 */
	public InitValueDTO(T value) {
		setAttr(new Attr(value));
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

}
