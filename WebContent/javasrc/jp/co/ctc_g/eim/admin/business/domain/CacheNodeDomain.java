package jp.co.ctc_g.eim.admin.business.domain;

/**
 * キャッシュノードドメイン
 *
 */
public class CacheNodeDomain {

	/** ノードID */
	private long id;

	/** ノード名称 */
	private String name;


	/**
	 * ノードIDを取得します。
	 * @return ノードID
	 */
	public long getId() {
		return id;
	}

	/**
	 * ノードIDを設定します。
	 * @param id ノードID
	 */
	public void setId(long id) {
		this.id = id;
	}

	/**
	 * ノード名称を取得します。
	 * @return ノード名称
	 */
	public String getName() {
		return name;
	}

	/**
	 * ノード名称を設定します。
	 * @param name ノード名称
	 */
	public void setName(String name) {
		this.name = name;
	}

}
