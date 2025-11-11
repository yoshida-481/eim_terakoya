package jp.co.ctc_g.eim.common.presentation.web.dto;

import java.util.ArrayList;
import java.util.List;

import org.springframework.util.StringUtils;

import jp.co.ctc_g.eim.framework.business.domain.AssignEntryDomain;
import jp.co.ctc_g.eim.framework.business.domain.BelongDomain;

/**
 * AssignEntryDTO
 * <p>
 * Struts->SpringMVCへの移行に際して、クライアントへ送信するXMLデータを再現したデータ転送用オブジェクトです。
 */
public class AssignEntryDTO<T> {

	/**
	 * Strutsで生成したXMLデータにおける配列要素を再現するためのクラスです。
	 */
	public static class AsEntryList {
		/** 名称リスト */
		@SuppressWarnings("rawtypes")
		private List<AssignEntryDTO> asEntry = new ArrayList<AssignEntryDTO>();

		/**
		 * コンストラクタ
		 * @throws Exception 
		 */
		@SuppressWarnings("deprecation")
		public AsEntryList(AssignEntryDomain assignEntryDomain) throws Exception {
			if (StringUtils.isEmpty(assignEntryDomain)){
				return;
			}
			for (BelongDomain belong : assignEntryDomain.getBelongList()) {
				asEntry.add(new AssignEntryDTO<Object>(belong));
			}
		}

		/**
		 * @return
		 */
		@SuppressWarnings("rawtypes")
		public List<AssignEntryDTO> getAsEntry() {
			return asEntry;
		}

		/**
		 * @param name
		 */
		@SuppressWarnings("rawtypes")
		public void setAsEntry(List<AssignEntryDTO> asEntry) {
			this.asEntry = asEntry;
		}

	}

	/**
	 * クライアントへ送信していたXMLデータにおいて、単数値のプロパティを保持する親要素を再現するためのクラスです。
	 */
	public class Attr {
		/** ID */
		private String id = null;
		/** タイプ */
		private String type = null;
		/** 名称 */
		private String name = null;

		private Attr(T domain) throws Exception {
			if (domain instanceof BelongDomain){
				setId(String.valueOf(((BelongDomain) domain).getBelonging().getId()));
				setType(((BelongDomain) domain).getBelongType().getSymbol());
				setName(((BelongDomain) domain).getBelonging().getName());
			}	
		}

		public String getId() {
			return id;
		}

		public void setId(String id) {
			this.id = id;
		}

		public String getType() {
			return type;
		}

		public void setType(String type) {
			this.type = type;
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
	 * コンストラクタ
	 *
	 * @param domain 
	 * @throws Exception
	 */
	public AssignEntryDTO(T domain) throws Exception {
		setAttr(new Attr(domain));
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
