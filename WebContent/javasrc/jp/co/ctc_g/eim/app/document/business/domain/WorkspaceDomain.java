package jp.co.ctc_g.eim.app.document.business.domain;

import java.util.List;

import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.common.enumeration.ValueTypeEnum;


/**
* 【ドキュメントAPI】
* ワークスペースドメイン
*@since Ver1.0
*/
public class WorkspaceDomain extends ObjectDomain implements PlaceDomain {


	/** 子フォルダリスト */
	private List<FolderDomain> folderList = null;

	/** 子ドキュメントリスト */
	private List<DocumentDomain> documentList = null;

	/**
	 * コンストラクタ
	 *
	 * @since Ver1.0
	 */
	public WorkspaceDomain(){

	}


	/**
	 * パスを取得します。
	 *
	 * @return パス
	 * @since Ver1.0
	 */

	public String getPath(){
		AttributeDomain attDomain = super.getAttribute("パス");
		if(attDomain == null){
			return null;
		}else if(attDomain.getStringList() == null || attDomain.getStringList().size() == 0){
			return null;
		}

		return attDomain.getStringList().get(0);
	}

	/**
	 * プロパティを取得します。
	 *
	 * @return プロパティ
	 * @since Ver1.0
	 */
	public String getProperty() {
		AttributeDomain attDomain = super.getAttribute("プロパティ");
		if(attDomain == null){
			return null;
		}

		return attDomain.getString();
	}

	/**
	 * プロパティを設定します。
	 *
	 * @param property プロパティ
	 * @since Ver1.0
	 */
	public void setProperty(String property) {

		if(property == null || property.length() == 0 || property.trim().length() == 0){
			//属性リストからの除外処理
			if(super.getAttribute("プロパティ") != null){
				List<AttributeDomain> attList = super.getAttributeList();
				for(int i=0;i<attList.size();i++){
					AttributeDomain attributeDomain = attList.get(i);
					if(attributeDomain != null){
						if(attributeDomain.getAttributeType().getDefinitionName().equals("プロパティ")){
							attList.remove(i);
							break;
						}
					}
				}
			}
		}else{

			if(super.getAttribute("プロパティ") == null){

				AttributeDomain attDomain = new AttributeDomain();
				AttributeTypeDomain attTypeDomain = new AttributeTypeDomain("プロパティ");
				attTypeDomain.setValueType(ValueTypeEnum.STRING);
				attDomain.setAttributeType(attTypeDomain);
				attDomain.setString(property);
				super.getAttributeList().add(attDomain);

			}else{
				super.getAttribute("プロパティ").setString(property);
			}
		}
	}

	/**
	 * 子ドキュメントリストを取得します。
	 *
	 * @return ドキュメントドメインリスト
	 * @since Ver1.0
	 */
	public List<DocumentDomain> getDocumentList() {
		return documentList;
	}

	/**
	 * 子ドキュメントリストを設定します。
	 *
	 * @param documentList ドキュメントドメインリスト
	 * @since Ver1.0
	 */
	public void setDocumentList(List<DocumentDomain> documentList) {
		this.documentList = documentList;
	}

	/**
	 * 子フォルダリストを取得します。
	 *
	 * @return フォルダドメインリスト
	 * @since Ver1.0
	 */
	public List<FolderDomain> getFolderList() {
		return folderList;
	}

	/**
	 * 子フォルダリストを設定します。
	 *
	 * @param folderList フォルダドメインリスト
	 * @since Ver1.0
	 */
	public void setFolderList(List<FolderDomain> folderList) {
		this.folderList = folderList;
	}

}
