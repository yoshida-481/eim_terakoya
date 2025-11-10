package jp.co.ctc_g.eim.common.presentation.web.dto;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * DefValueDTO
 * <p>
 * Struts->SpringMVCへの移行に際して、クライアントへ送信するXMLデータを再現したデータ転送用オブジェクトです。
 */
public class DefValueDTO<T> {

	/**
	 * Strutsで生成したXMLデータにおける配列要素を再現するためのクラスです。
	 */
	public static class DefValueList {

		@SuppressWarnings("rawtypes")
		private List<DefValueDTO> defValue = new ArrayList<DefValueDTO>();

		/**
		 * コンストラクタ
		 * @param defLongValueList 
		 * @param defStringValueList 
		 * @param defTextValueList 
		 * @param defDateValueList 
		 * @param defDoubleValueList 
		 */
		@SuppressWarnings({ "unchecked", "rawtypes" })
		public DefValueList(List<Long> defLongValueList, List<String> defStringValueList, List<String> defTextValueList, 
				List<Date> defDateValueList, List<Double> defDoubleValueList) {
			for (Long value : defLongValueList) {
				defValue.add(new DefValueDTO(value));
			}
			for (String value : defStringValueList) {
				defValue.add(new DefValueDTO(value));
			}
			for (String value : defTextValueList) {
				defValue.add(new DefValueDTO(value));
			}
			for (Date value : defDateValueList) {
				defValue.add(new DefValueDTO(value));
			}
			for (Double value : defDoubleValueList) {
				defValue.add(new DefValueDTO(value));
			}
		}

		@SuppressWarnings("rawtypes")
		public List<DefValueDTO> getDefValue() {
			return defValue;
		}

		@SuppressWarnings("rawtypes")
		public void setDefValue(List<DefValueDTO> defValue) {
			this.defValue = defValue;
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
	public DefValueDTO(T value) {
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
