package jp.co.ctc_g.eim.admin.presentation.web.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * キャッシュリロードDTO
 *
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class CacheReloadDTO {


	/** キャッシュ領域キー */
	private String cacheSpaceKey;

	/**
	 * リロードするキャッシュエントリーのPK一覧
	 * (FileDTOの場合のみObject ID + ":" + Format ID、それ以外のDTOはObject ID)
	 */
	private List<String> pkList;

	/**
	 * キャッシュ領域キーを取得します。
	 * @return キャッシュ領域キー
	 */
	public String getCacheSpaceKey() {
		return cacheSpaceKey;
	}

	/**
	 * キャッシュ領域キーを設定します。
	 * @param cacheSpaceKey キャッシュ領域キー
	 */
	public void setCacheSpaceKey(String cacheSpaceKey) {
		this.cacheSpaceKey = cacheSpaceKey;
	}

	/**
	 * キャッシュエントリーのPK一覧を取得します。
	 * @return キャッシュエントリーのPK一覧
	 */
	public List<String> getPkList() {
		return pkList;
	}

	/**
	 * キャッシュエントリーのPK一覧を設定します。
	 * @param pkList キャッシュエントリーのPK一覧
	 */
	public void setPkList(List<String> pkList) {
		this.pkList = pkList;
	}

}
