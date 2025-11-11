package jp.co.ctc_g.eim.admin.integration.dao.impl;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.SimpleClientHttpRequestFactory;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import jp.co.ctc_g.eim.admin.business.dao.CacheMonitorViewDao;
import jp.co.ctc_g.eim.admin.business.domain.CacheMonitorNodeDomain;
import jp.co.ctc_g.eim.admin.business.domain.CacheNodeDomain;
import jp.co.ctc_g.eim.admin.business.domain.criteria.CacheEntrySearchCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.cache.CacheEntryDomain;
import jp.co.ctc_g.eim.framework2.business.domain.cache.CacheEntrySearchDomain;
import jp.co.ctc_g.eim.framework2.business.domain.cache.CacheSpaceDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.DirectoryDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FormatDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.GroupDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.common.exception.EIMApplicationException;
import jp.co.ctc_g.eim.framework2.common.util.ResourceUtils;
import jp.co.ctc_g.eim.framework2.integration.dao.cache.DataCacheDao;
import jp.co.ctc_g.eim.framework2.integration.dao.domain.SynchronizeLogDomain;
import jp.co.ctc_g.eim.framework2.integration.dao.enumeration.CacheTypeEnum;
import jp.co.ctc_g.eim.framework2.integration.dao.util.CacheUtils;
import jp.co.ctc_g.eim.framework2.integration.dao.util.XMLDocumentLoaderUtil;
import jp.co.ctc_g.eim.framework2.presentation.web.dto.CacheEntryDTO;
import jp.co.ctc_g.eim.framework2.presentation.web.dto.CacheEntrySearchDTO;
import jp.co.ctc_g.eim.framework2.presentation.web.dto.CacheSpaceDTO;

/**
 * キャッシュモニター表示用のDAOクラス
 * @see jp.co.ctc_g.eim.admin.business.dao.CacheMonitorViewDao
 */
public class CacheMonitorViewDaoImpl implements CacheMonitorViewDao {

	/** キャッシュモニタービュー設定ファイルデフォルトパス */
	private static String CONF_FILE_DEFAULT_PATH = "/CacheMonitorConf.xml";

	/** キャッシュ一覧取得APIのURL */
	private static String FRAMEWORK_CACHES_URL = "/cache-monitor/caches.mvc";

	/** キャッシュエントリ検索APIのURL */
	private static String FRAMEWORK_ENTRIES_SEARCH_URL = "/cache-monitor/entries/search.mvc";

	/** ネームスペース設定ァイルパス */
	private String cacheMonitorConfFilePath;

	/** XMLファイル情報 */
	private org.w3c.dom.Document doc = null;

	/** ネームスペース一覧 */
	private List<CacheMonitorNodeDomain> cacheMonitorDomainList = null;


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
	 */
	public void init() throws Exception {
		// XMLファイルリストがNULL又は空白の場合、デフォルトパスを設定
		if (StringUtils.isBlank(cacheMonitorConfFilePath)) {
			cacheMonitorConfFilePath = CONF_FILE_DEFAULT_PATH;
		}

		/*
		 * ノード定義ファイルの読み込み
		 *
		 * WebContent/javasrc/CacheMonitorConf.xml
		 *
		 * <?xml version="1.0" encoding="UTF-8"?>
		 *
		 * <cacheMonitorConf>
		 *     <cacheMonitorList>
		 *         <node id="123456" name="システム管理(1号機)" scheme="http" host="XXX.XXX.XXX.XXX" port="XXXX" path="/eim/rest"/>
		 *         <node id="234567" name="システム管理(2号機)" scheme="http" host="XXX.XXX.XXX.XXX" port="XXXX" path="/eim/rest" />
		 *         <node id="345678" name="システム管理(3号機)" scheme="http" host="XXX.XXX.XXX.XXX" port="XXXX" path="/eim/rest" />
		 *         <node id="456789" name="オブジェクトエディタ(1号機)" scheme="http" host="XXX.XXX.XXX.XXX" port="XXXX" path="/oe/rest" />
		 *         <node id="912345" name="オブジェクトエディタ(2号機)" scheme="http" host="XXX.XXX.XXX.XXX" port="XXXX" path="/oe/rest" />
		 *         <node id="987654" name="オブジェクトエディタ(3号機)" scheme="http" host="XXX.XXX.XXX.XXX" port="XXXX" path="/oe/rest" />
		 *         <node id="876543" name="アプリケーション(1号機)" scheme="http" host="XXX.XXX.XXX.XXX" port="XXXX" path="/app/api" />
		 *         <node id="765432" name="アプリケーション(2号機)" scheme="http" host="XXX.XXX.XXX.XXX" port="XXXX" path="/app/api" />
		 *         <node id="654321" name="アプリケーション(3号機)" scheme="http" host="XXX.XXX.XXX.XXX" port="XXXX" path="/app/api" />
		 *     </cacheMonitorList>
		 * </cacheMonitorConf>
		 */
		doc = XMLDocumentLoaderUtil.getDocument(cacheMonitorConfFilePath);

		cacheMonitorDomainList = new ArrayList<CacheMonitorNodeDomain>();
		// ルート要素を取得（タグ名：cacheMonitorConf）
		Element root = doc.getDocumentElement();

		// namespace要素のリストを取得
		NodeList cacheMonitorNodeList = root.getElementsByTagName("node");

		// mailType要素の数だけループ
		for (int i=0; i<cacheMonitorNodeList.getLength(); i++) {
			Element cacheMonitorNodeElement = (Element)cacheMonitorNodeList.item(i);

			CacheMonitorNodeDomain cacheMonitorNodeDomain = new CacheMonitorNodeDomain();
			cacheMonitorNodeDomain.setId(Long.parseLong(cacheMonitorNodeElement.getAttribute("id")));
			cacheMonitorNodeDomain.setName(cacheMonitorNodeElement.getAttribute("name"));
			cacheMonitorNodeDomain.setScheme(cacheMonitorNodeElement.getAttribute("scheme"));
			cacheMonitorNodeDomain.setHost(cacheMonitorNodeElement.getAttribute("host"));
			cacheMonitorNodeDomain.setPort(cacheMonitorNodeElement.getAttribute("port"));
			cacheMonitorNodeDomain.setPath(cacheMonitorNodeElement.getAttribute("path"));
			cacheMonitorDomainList.add(cacheMonitorNodeDomain);
		}
	}

	/**
	 * (非 Javadoc)
	 * @see jp.co.ctc_g.eim.admin.business.dao.CacheMonitorViewDao#getNodes()
	 */
	public List<CacheNodeDomain> getNodes() throws Exception {
		// ノード定義に設定されている一覧を取得
		List<CacheNodeDomain> nodeDomainList = new ArrayList<CacheNodeDomain>();

		for (CacheMonitorNodeDomain cacheMonitorNodeDomain : cacheMonitorDomainList) {
			CacheNodeDomain nodeDomain = new CacheNodeDomain();
			nodeDomain.setId(cacheMonitorNodeDomain.getId());
			nodeDomain.setName(cacheMonitorNodeDomain.getName());
			nodeDomainList.add(nodeDomain);
		}

		return nodeDomainList;
	}

	/**
	 * (非 Javadoc)
	 * @see jp.co.ctc_g.eim.admin.business.dao.CacheMonitorViewDao#getCaches(long nodeId)
	 */
	public List<CacheSpaceDomain> getCaches(long nodeId) throws Exception {

		URI uri = getURI(nodeId, FRAMEWORK_CACHES_URL);

		// infoLog.info("リクエスト送信先 URI :" + uri.toString());

		// レストテンプレートを生成する
		RestTemplate client = new RestTemplate(new SimpleClientHttpRequestFactory());

		// ヘッダ作成
		HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_JSON);
		headers.set("ACCESS_KEY", "EIMANAGER");

		// リクエスト作成
		RequestEntity<?> req = new RequestEntity<>(headers, HttpMethod.GET, uri);

		ResponseEntity<List<CacheSpaceDTO>> res;
		try {
			// リクエスト処理（第二引数は受信したボディの変換先の型）
			res = client.exchange(req, new ParameterizedTypeReference<List<CacheSpaceDTO>>(){});
		} catch (RestClientException e) {
			throw new EIMApplicationException("EIM.ERROR.LOGIC.NO.NODE.RESPONSE");
		}
		List<CacheSpaceDTO> result = res.getBody();

		List<CacheSpaceDomain> spaceDomainList = new ArrayList<CacheSpaceDomain>();
		for (CacheSpaceDTO spaceDTO : result) {
			CacheSpaceDomain spaceDomain = new CacheSpaceDomain();
			spaceDomain.setKey(spaceDTO.getKey());
			spaceDomain.setName(spaceDTO.getName());
			spaceDomainList.add(spaceDomain);
		}

		return spaceDomainList;
	}

	/**
	 * (非 Javadoc)
	 * @see jp.co.ctc_g.eim.admin.business.dao.CacheMonitorViewDao#searchEntries(CacheEntrySearchCriteria cacheEntrySearchCriteria)
	 */
	public CacheEntrySearchDomain searchEntries(CacheEntrySearchCriteria cacheEntrySearchCriteria) throws Exception {

		// URL作成
		URI uri = getURI(cacheEntrySearchCriteria.getNodeId(), FRAMEWORK_ENTRIES_SEARCH_URL);

		// infoLog.info("リクエスト送信先 URI :" + uri.toString());

		// レストテンプレートを生成する
		RestTemplate client = new RestTemplate(new SimpleClientHttpRequestFactory());

		// ヘッダ作成
		HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_JSON);
		headers.set("ACCESS_KEY", "EIMANAGER");

		// リクエスト作成
		RequestEntity<?> req = new RequestEntity<>(cacheEntrySearchCriteria, headers, HttpMethod.POST, uri);

		ResponseEntity<CacheEntrySearchDTO> res;
		try {
			// リクエスト処理（第二引数は受信したボディの変換先の型）
			res = client.exchange(req, CacheEntrySearchDTO.class);
		} catch (RestClientException e) {
			throw new EIMApplicationException("EIM.ERROR.LOGIC.NO.NODE.RESPONSE");
		}
		CacheEntrySearchDTO result = res.getBody();

		CacheEntrySearchDomain cacheEntrySearchDomain = new CacheEntrySearchDomain();
		cacheEntrySearchDomain.setAllCounts(result.getAllCounts());
		cacheEntrySearchDomain.setAttributeTypeList(result.getAttributeTypeList());
		for (CacheEntryDTO cacheEntryDTO : result.getEntryList()) {
			CacheEntryDomain cacheEntryDomain = new CacheEntryDomain();
			cacheEntryDomain.setReloading(cacheEntryDTO.isReloading());
			cacheEntryDomain.setEntry(cacheEntryDTO.getEntry());
			cacheEntryDomain.setAttributeValueMap(cacheEntryDTO.getAttributeValueMap());
			cacheEntrySearchDomain.getEntryList().add(cacheEntryDomain);
		}

		return cacheEntrySearchDomain;
	}

	/**
	 * (非 Javadoc)
	 * @see jp.co.ctc_g.eim.admin.business.dao.CacheMonitorViewDao#reload(String cacheSpaceKey, List<String> pkList)
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	public void reload(String cacheSpaceKey, List<String> pkList) throws Exception {
		DataCacheDao dao = null;
		Class domainClass = null;
		if ( CacheTypeEnum.FILE.getName().equals(cacheSpaceKey) ) {
			// ファイル
			dao = (DataCacheDao) CacheUtils.getFileCacheDao();
			domainClass = FileDomain.class;

		} else if ( CacheTypeEnum.OBJECT_TYPE.getName().equals(cacheSpaceKey) ) {
			// オブジェクトタイプ
			dao = (DataCacheDao) CacheUtils.getObjectTypeCacheDao();
			domainClass = ObjectTypeDomain.class;

		} else if ( CacheTypeEnum.ATTRIBUTE_TYPE.getName().equals(cacheSpaceKey) ) {
			// 属性タイプ
			dao = CacheUtils.getAttributeTypeCacheDao();
			domainClass = AttributeTypeDomain.class;

		} else if ( CacheTypeEnum.FORMAT.getName().equals(cacheSpaceKey) ) {
			// フォーマット
			dao = CacheUtils.getFormatCacheDao();
			domainClass = FormatDomain.class;

		} else if ( CacheTypeEnum.DIRECTORY.getName().equals(cacheSpaceKey) ) {
			// ディレクトリ
			dao = CacheUtils.getDirectoryCacheDao();
			domainClass = DirectoryDomain.class;

		} else if ( CacheTypeEnum.USER.getName().equals(cacheSpaceKey) ) {
			// ユーザ
			dao = CacheUtils.getUserCacheDao();
			domainClass = UserDomain.class;

		} else if ( CacheTypeEnum.GROUP.getName().equals(cacheSpaceKey) ) {
			// グループ
			dao = CacheUtils.getGroupCacheDao();
			domainClass = GroupDomain.class;

		} else {
			// オブジェクト
			dao = (DataCacheDao) CacheUtils.getObjectCacheDao();
			domainClass = ObjectDomain.class;

		}

		List updateLogList = new ArrayList<>();
		for (String pk : pkList) {
			SynchronizeLogDomain updateLog = new SynchronizeLogDomain();
			updateLog.setType(domainClass);
			if (CacheTypeEnum.FILE.getName().equals(cacheSpaceKey)) {
				updateLog.setPrimaryKey(pk);
			} else {
				updateLog.setPrimaryKey(Long.parseLong(pk));
			}
			updateLogList.add(updateLog);
		}
		dao.putUpdateLogAllFromSynchronizeLogDomainList(updateLogList);

		return;
	}

	/**
	 * キャッシュモニタービュー設定ファイルパスを取得します。
	 * @return キャッシュモニタービュー設定ファイルパス
	 */
	public String getCacheMonitorConfFilePath() {
		return cacheMonitorConfFilePath;
	}

	/**
	 * キャッシュモニタービュー設定ファイルパスを設定します。
	 * @param cacheMonitorConfFilePath キャッシュモニタービュー設定ファイルパス
	 */
	public void setCacheMonitorConfFilePath(String cacheMonitorConfFilePath) {
		this.cacheMonitorConfFilePath = cacheMonitorConfFilePath;
	}

	/**
	 * URIを取得します。
	 * @param nodeId ノードID
	 * @param apiPath APIのパス（http://localhost:8080/○○○/cache-monitor/...の○○○の部分）
	 * @return URI
	 */
	protected URI getURI(long nodeId, String apiPath) throws Exception {
		// ノード一覧から該当ノードを取得
		CacheMonitorNodeDomain cacheMonitorNodeDomain = null;
		for (int i = 0; i < cacheMonitorDomainList.size(); i++) {
			if (nodeId == cacheMonitorDomainList.get(i).getId()) {
				cacheMonitorNodeDomain = cacheMonitorDomainList.get(i);
			}
		}
		if (cacheMonitorNodeDomain == null) {
			// nodeId不正
			String message = ResourceUtils.getByKey("EIM.ERROR.LOGIC.PLUGIN.NG", new Object[]{"nodeId"});
			throw new Exception(message);

		}

		// URL作成
		String scheme = cacheMonitorNodeDomain.getScheme();
		String host = cacheMonitorNodeDomain.getHost();
		int port = Integer.parseInt(cacheMonitorNodeDomain.getPort());
		String path = cacheMonitorNodeDomain.getPath() + apiPath;
		URI uri = new URI(scheme, null, host, port, path, null, null);

		return uri;
	}


}
