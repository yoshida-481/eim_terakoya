package jp.co.ctc_g.eim.admin.presentation.web.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * キャッシュノードDTO
 *
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class CacheNodeDTO {

	/** ノードID */
	private long id;

	/** ノード名 */
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
	 * ノード名を取得します。
	 * @return ノード名
	 */
	public String getName() {
		return name;
	}

	/**
	 * ノード名を設定します。
	 * @param name ノード名
	 */
	public void setName(String name) {
		this.name = name;
	}

}
