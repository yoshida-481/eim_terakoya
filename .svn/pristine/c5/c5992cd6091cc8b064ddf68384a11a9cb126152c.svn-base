package jp.co.ctc_g.eim.admin.integration.dao.impl.xml;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import eim.bo.EIMException;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import jp.co.ctc_g.eim.admin.business.dao.NamespaceDao;
import jp.co.ctc_g.eim.admin.business.domain.NamespaceDomain;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.integration.dao.util.XMLDocumentLoaderUtil;

/**
 * ネームスペース設定のDAOクラス
 * @see jp.co.ctc_g.eim.admin.business.dao.NamespaceDao
 */
public class NamespaceDaoImpl implements NamespaceDao {

	/** ネームスペース一覧 */
	private List<NamespaceDomain> namespaceDomainList = null;

	/** ネームスペース設定ァイルパス */
	private String namespaceConfFilePath;

	/** XMLファイル情報 */
	private org.w3c.dom.Document doc = null;

	/** ネームスペース設定ファイルデフォルトパス */
	private static String CONF_FILE_DEFAULT_PATH = "/NamespaceConf.xml";

	/**
	 * 初期化処理<br>
	 * XMLドキュメント設定ファイルの解析結果をdocに保持します。<br>
	 *
	 * @throws Exception 以下場合、例外を通知します。<br>
	 * <ul>
	 * 	<li>指定したXMLファイルが見つかりません。</li>
	 * 	<li>読込んだXMLのnameに禁則文字が含まれています。</li>
	 * </ul>
	 * <br>
	 * @since Ver6.0
	 */
	public void init() throws Exception {

		// XMLファイルリストがNULL又は空白の場合、デフォルトパスを設定
		if(StringUtils.isBlank(namespaceConfFilePath)){
			namespaceConfFilePath = CONF_FILE_DEFAULT_PATH;
		}

		doc = XMLDocumentLoaderUtil.getDocument(namespaceConfFilePath);

		namespaceDomainList = new ArrayList<NamespaceDomain>();
		// ルート要素を取得（タグ名：namespaceConf）
		Element root = doc.getDocumentElement();

		// namespace要素のリストを取得
		NodeList namespaceNodeList = root.getElementsByTagName("namespace");

		// mailType要素の数だけループ
		for (int i = 0; i < namespaceNodeList.getLength() ; i++){
			NamespaceDomain namespaceDomain = new NamespaceDomain();
			Element namespaceElement = (Element)namespaceNodeList.item(i);
			namespaceDomain.setName(namespaceElement.getAttribute("name"));
//			namespaceDomain.setDescription(namespaceElement.getAttribute("description"));
			namespaceDomainList.add(namespaceDomain);
		}

		// XML内容の一意制約チェック、禁則文字チェック
		checkXml();
	}


	/**
	 * ネームスペース設定ファイルよりネームスペース一覧を返します。
	 *
	 * @see jp.co.ctc_g.eim.admin.business.dao.NamespaceDao#getList()
	 * @since Ver6.0
	 */
	public List<NamespaceDomain> getList() throws Exception {

		if (namespaceDomainList != null) {
			return namespaceDomainList;
		}

		//読み込み
		init();

		return namespaceDomainList;

	}

	/**
	 * 禁則文字チェックをします。
	 *
	 */
	private void checkXml() throws Exception {

		EIMSession sess = EIMThreadContext.getEIMSession();
		List<String> nameList = new ArrayList<String>();

		for(NamespaceDomain namespace : namespaceDomainList){

			if(namespace.getName().indexOf(EIMConfig.get("NAMESPACE_DIVID_CHAR")) != -1){
				namespaceDomainList = null;
				// ネームスペースの設定ファイルのname "{0}" に禁則文字"{1}"が含まれています。
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NAMESPACEXML.PROHIBITIONCHAR.EXISTS", new Object[]{namespace.getName(), EIMConfig.get("NAMESPACE_DIVID_CHAR")});
			}

			nameList.add(namespace.getName());
		}

	}

	/**
	 * ネームスペース一覧より指定したネームスペース名のネームスペースを返します。
	 *
	 * @see jp.co.ctc_g.eim.admin.business.dao.NamespaceDao#getById(String)
	 * @since Ver6.0
	 */
	public NamespaceDomain getByNamespaceName(String namespaceName) throws Exception {

		namespaceDomainList = getList();

		for(NamespaceDomain namespaceDomain:namespaceDomainList){
			if(namespaceDomain.getName().equals(namespaceName)){
				return namespaceDomain;
			}
		}
		return null;
	}

	/**
	 * ネームスペース設定ファイルパスを取得します。
	 * @return ネームスペース設定ファイルパス
	 */
	public String getNamespaceConfFilePath() {
	    return namespaceConfFilePath;
	}

	/**
	 * ネームスペース設定ファイルパスを設定します。
	 * @param namespaceConfFilePath ネームスペース設定ファイルパス
	 */
	public void setNamespaceConfFilePath(String namespaceConfFilePath) {
		this.namespaceConfFilePath = namespaceConfFilePath;
	}

}
