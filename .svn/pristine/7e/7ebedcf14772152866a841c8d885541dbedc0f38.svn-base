package jp.co.ctc_g.eim.common.presentation.web.dto;

import java.util.ArrayList;
import java.util.List;

import jp.co.ctc_g.eim.framework.business.domain.BaseEventTypeDomain;

/**
 * BaseEventTypeDTO
 * <p>
 * Struts->SpringMVCへの移行に際して、クライアントへ送信するXMLデータを再現したデータ転送用オブジェクトです。
 */
public class BaseEventTypeDTO {

	/**
	 * Strutsで生成したXMLデータにおける配列要素を再現するためのクラスです。
	 */
	public static class BaseEventTypeList {

		private List<BaseEventTypeDTO> baseEventType = new ArrayList<BaseEventTypeDTO>();

		/**
		 * コンストラクタ
		 * 
		 * @param baseEventTypeList
		 */
		public BaseEventTypeList(List<BaseEventTypeDomain> baseEventTypeList) {
			for (BaseEventTypeDomain baseEventTypeDomain : baseEventTypeList) {
				baseEventType.add(new BaseEventTypeDTO(baseEventTypeDomain));
			}
		}

		public List<BaseEventTypeDTO> getBaseEventType() {
			return baseEventType;
		}

		public void setBaseEventType(List<BaseEventTypeDTO> baseEventType) {
			this.baseEventType = baseEventType;
		}
	}

	/**
	 * クライアントへ送信していたXMLデータにおいて、単数値のプロパティを保持する親要素を再現するためのクラスです。
	 */
	public class Attr {
		private String id = null;
		private String name = null;

		/**
		 * コンストラクタ
		 * 
		 * @param baseEventTypeDomain
		 */
		private Attr(BaseEventTypeDomain baseEventTypeDomain) {
			id = String.valueOf(baseEventTypeDomain.getId());
			name = baseEventTypeDomain.getName();
		}

		public String getId() {
			return id;
		}

		public void setId(String id) {
			this.id = id;
		}

		public String getName() {
			return name;
		}

		public void setName(String name) {
			this.name = name;
		}
	}

	/**
	 * Strutsで生成したXMLデータにおいて単数値のプロパティを保持する親要素です。
	 */
	private Attr attr = null;

	/**
	 * コンストラクタ<br>
	 * BaseEventTypeDomainが保持しているプロパティ値を設定します。<br>
	 *
	 * @param baseEventTypeDomain 
	 */
	public BaseEventTypeDTO(BaseEventTypeDomain baseEventTypeDomain) {
		setAttr(new Attr(baseEventTypeDomain));
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
