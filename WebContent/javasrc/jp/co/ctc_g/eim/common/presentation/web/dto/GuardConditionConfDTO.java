package jp.co.ctc_g.eim.common.presentation.web.dto;

import java.util.ArrayList;
import java.util.List;

import jp.co.ctc_g.eim.framework.business.domain.GuardConditionDomain;

/**
 * GuardConditionConfDTO
 * <p>
 * Struts->SpringMVCへの移行に際して、クライアントへ送信するXMLデータを再現したデータ転送用オブジェクトです。
 */
public class GuardConditionConfDTO {

	/**
	 * Strutsで生成したXMLデータにおける配列要素を再現するためのクラスです。
	 */
	public static class GuardConditionList {

		private List<GuardConditionConfDTO> guardCondition = new ArrayList<GuardConditionConfDTO>();

		/**
		 * コンストラクタ
		 * 
		 * @param guardConditionList
		 */
		public GuardConditionList(List<GuardConditionDomain> guardConditionList) {
			for (GuardConditionDomain GuardConditionDomain : guardConditionList) {
				guardCondition.add(new GuardConditionConfDTO(GuardConditionDomain));
			}
		}

		public List<GuardConditionConfDTO> getGuardCondition() {
			return guardCondition;
		}

		public void setGuardCondition(List<GuardConditionConfDTO> guardCondition) {
			this.guardCondition = guardCondition;
		}
	}

	/**
	 * クライアントへ送信していたXMLデータにおいて、単数値のプロパティを保持する親要素を再現するためのクラスです。
	 */
	public class Attr {
		/** ID */
		private String id = null;

		/** 名称 */
		private String name = null;

		/**
		 * コンストラクタ
		 * 
		 * @param GuardConditionDomain
		 */
		private Attr(GuardConditionDomain GuardConditionDomain) {
			id = String.valueOf(GuardConditionDomain.getId());
			name = GuardConditionDomain.getName();
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
	 * GuardConditionDomainが保持しているプロパティ値を設定します。<br>
	 *
	 * @param GuardConditionDomain
	 */
	public GuardConditionConfDTO(GuardConditionDomain GuardConditionDomain) {
		setAttr(new Attr(GuardConditionDomain));
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
