package jp.co.ctc_g.eim.app.document.business.domain;
/**
* 【ドキュメントAPI】
* 場所ドメイン
*@since Ver1.0
*/
public interface PlaceDomain{

	public long getId();

	public String getName();

	public String getPath();
	
	public void setId(long id);

	public void setName(String name);
}