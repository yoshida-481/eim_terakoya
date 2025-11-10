package jp.co.ctc_g.eim.app.document.business.domain;

import java.util.Date;
import java.util.List;

import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.common.enumeration.ValueTypeEnum;


/**
* 【ドキュメントAPI】
* ドキュメントドメイン
*@since Ver1.0
*/
public class DocumentDomain extends ObjectDomain {

	/** ファイルサイズ */
	private long size = 0;
	
	/** 作成者 **/
	private UserDomain createUser = null;

	/**
	 * コンストラクタ
	 * 
	 * @since Ver1.0
	 */
	public DocumentDomain(){
		
	}
	
	
	/**
	 * ファイルサイズを取得します。
	 * 
	 * @return ファイルサイズ
	 * @since Ver1.0
	 */
	public long getSize() {
		return size;
	}

	/**
	 * ファイルサイズを設定します。(属性更新の対象外)
	 * 
	 * @param size ファイルサイズ
	 * @since Ver1.0
	 */
	public void setSize(long size) {
		this.size = size;
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
		
		AttributeDomain attDomain = null;
		
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
			//プロパティ設定
			if(super.getAttribute("プロパティ") == null){
				
				attDomain = new AttributeDomain();
				AttributeTypeDomain attTypeDomain = new AttributeTypeDomain("プロパティ");
				attTypeDomain.setValueType(ValueTypeEnum.STRING);
				attDomain.setAttributeType(attTypeDomain);
				attDomain.setString(property);
				super.getAttributeList().add(attDomain);
			
			}else{
				attDomain = super.getAttribute("プロパティ");
				attDomain.getStringList().clear();
				attDomain.setString(property);
			}
		}
	}
	
	/**
	 * 有効期限を取得します。
	 * 
	 * @return 有効期限
	 * @since Ver1.0
	 */
	public Date getExpirationDate() {
		
		AttributeDomain attDomain = super.getAttribute("有効期限");
		if(attDomain == null){
			return null;
		}
		
		return attDomain.getDate();
	}

	/**
	 * 有効期限を設定します。
	 * 
	 * @param expirationDate 有効期限
	 * @since Ver1.0
	 */
	public void setExpirationDate(Date expirationDate) {
		
		AttributeDomain attDomain = null;
		
		if(expirationDate == null){
			//属性リストからの除外処理
			if(super.getAttribute("有効期限") != null){
				List<AttributeDomain> attList = super.getAttributeList();
				for(int i=0;i<attList.size();i++){
					AttributeDomain attributeDomain = attList.get(i);
					if(attributeDomain != null){
						if(attributeDomain.getAttributeType().getDefinitionName().equals("有効期限")){
							attList.remove(i);
							break;
						}
					}
				}
			}
		}else{
			if(super.getAttribute("有効期限") == null){
				attDomain = new AttributeDomain();
				AttributeTypeDomain attTypeDomain = new AttributeTypeDomain("有効期限");
				attTypeDomain.setValueType(ValueTypeEnum.DATE);
				attDomain.setAttributeType(attTypeDomain);
				attDomain.setDate(expirationDate);
				super.getAttributeList().add(attDomain);
			
			}else{
				attDomain = super.getAttribute("有効期限");
				attDomain.getDateList().clear();
				attDomain.setDate(expirationDate);
			}
		}
	}
	
	/**
	 * 作成者を取得します。
	 * 
	 * @return 作成者
	 * @since Ver1.0
	 */
	public UserDomain getCreateUser() {

		return this.createUser;
	}
	
	/**
	 * 作成者を設定します。
	 * 
	 * @param createUser 作成者
	 * @since Ver1.0
	 */
	public void setCreateUser(UserDomain createUser) {
		
		this.createUser = createUser;
	}
	
	
	/**
	 * 改訂内容を取得します。
	 * 
	 * @return 改訂内容
	 * @since Ver1.0
	 */
	public String getRevisedContent() {
		AttributeDomain attDomain = super.getAttribute("改訂内容");
		if(attDomain == null){
			return null;
		}
		
		return attDomain.getString();
	}
	
	/**
	 * 改訂内容を設定します。
	 * 
	 * @param revisedContent 改訂内容
	 * @since Ver1.0
	 */
	public void setRevisedContent(String revisedContent) {
		
		AttributeDomain attDomain = null;
		
		if(revisedContent == null || revisedContent.length() == 0 || revisedContent.trim().length() == 0){
			//属性リストからの除外処理
			if(super.getAttribute("改訂内容") != null){
				List<AttributeDomain> attList = super.getAttributeList();
				for(int i=0;i<attList.size();i++){
					AttributeDomain attributeDomain = attList.get(i);
					if(attributeDomain != null){
						if(attributeDomain.getAttributeType().getDefinitionName().equals("改訂内容")){
							attList.remove(i);
							break;
						}
					}
				}
			}
		}else{
			if(super.getAttribute("改訂内容") == null){
				attDomain = new AttributeDomain();
				AttributeTypeDomain attTypeDomain = new AttributeTypeDomain("改訂内容");
				attDomain.setAttributeType(attTypeDomain);
				attDomain.setString(revisedContent);
				super.getAttributeList().add(attDomain);
			
			}else{
				attDomain = super.getAttribute("改訂内容");
				attDomain.getStringList().clear();
				attDomain.setString(revisedContent);
			}
		}
	}

	
	
}
