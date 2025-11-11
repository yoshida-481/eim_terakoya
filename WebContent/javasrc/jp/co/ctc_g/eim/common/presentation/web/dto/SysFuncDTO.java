package jp.co.ctc_g.eim.common.presentation.web.dto;

import java.util.ArrayList;
import java.util.List;

import jp.co.ctc_g.eim.framework.business.domain.SysFuncDomain;

/**
 * SysFuncDTO
 * <p>
 * Struts->SpringMVCへの移行に際して、クライアントへ送信するXMLデータを再現したデータ転送用オブジェクトです。
 */
public class SysFuncDTO {

	/**
	 * Strutsで生成したXMLデータにおける配列要素を再現するためのクラスです。
	 */
	public static class SysFuncList {

		private List<SysFuncDTO> sysFunc = new ArrayList<SysFuncDTO>();

		/**
		 * コンストラクタ
		 * 
		 * @param sysFuncDomain
		 */
		public SysFuncList(List<SysFuncDomain> sysFuncList) {
			for (SysFuncDomain sysFuncDomain : sysFuncList) {
				sysFunc.add(new SysFuncDTO(sysFuncDomain));
			}
		}

		/**
		 * @return
		 */
		public List<SysFuncDTO> getSysFunc() {
			return sysFunc;
		}

		/**
		 * @param name
		 */
		public void setSysFunc(List<SysFuncDTO> sysFunc) {
			this.sysFunc = sysFunc;
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
		 * @param sysFuncDomain
		 */
		private Attr(SysFuncDomain sysFuncDomain) {
			id = String.valueOf(sysFuncDomain.getId());
			name = sysFuncDomain.getName();
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
	 * sysFuncDomainが保持しているプロパティ値を設定します。<br>
	 *
	 * @param sysFuncDomain
	 */
	public SysFuncDTO(SysFuncDomain sysFuncDomain) {
		setAttr(new Attr(sysFuncDomain));
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
