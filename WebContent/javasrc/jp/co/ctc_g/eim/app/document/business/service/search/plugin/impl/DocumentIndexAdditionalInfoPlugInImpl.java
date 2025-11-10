package jp.co.ctc_g.eim.app.document.business.service.search.plugin.impl;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import common.util.AppSqlUtil;
import eim.bo.EIMObject;
import eim.bo.EIMSearchConditionIn;
import eim.bo.EIMSearchLimitCountCondition;
import eim.bo.EIMSearchResultList;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMSearchSelectEIMObject.SearchConditionBuildHelper;
import eim.net.EIMSession;
import eim.util.SearchUtils;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.search.solr.indexBase.business.service.plugin.impl.FileIndexAdditionalInfoPlugInImpl;

/**
 * ドキュメントインデックス生成時に必要な、ドキュメントが保持するファイル情報とステータスなし公開ドキュメント判定情報
 * の一括取得を行うプラグインクラスです。
 */
public class DocumentIndexAdditionalInfoPlugInImpl extends FileIndexAdditionalInfoPlugInImpl {

	/**
	 * ドキュメントインデックス生成時に必要な、ドキュメントが保持するファイル情報とステータスなし公開ドキュメント判定情報
	 * の一括取得を行い保持するクラスです。
	 */
	public class DocumentIndexAdditionalInfo extends FileIndexAdditionalInfo {

		/** ステータスなし公開ドキュメントIDのSet */
		private Set<Long> noSTPublicObjSet = new HashSet<>();

		/**
		 * ステータスなし公開オブジェクトを取得します。 (JSPからの流用)
		 * @param objectList
		 * @throws Exception
		 */
		private void initNoStatusPulicObject(List<EIMObject> objectList) throws Exception {

			if (objectList.size() == 0) {
				return;
			}

			EIMSession sess = EIMThreadContext.getEIMSession();

			// ステータスなしの「改定中 過去レビジョン全て」「改定なし」「初期登録」を取得
			// これに非該当のドキュメントは公開アイコンを表示しない
			List<Long> objectIdList = objectList.stream().map(obj -> new Long(obj.getId())).collect(Collectors.toList());
			String sql = AppSqlUtil.getSqlNoStatusPublicIconObj(objectIdList);

			SearchConditionBuildHelper h = new SearchConditionBuildHelper();
			EIMSearchSelectEIMObject noSTEditObjSelect = new EIMSearchSelectEIMObject();
			noSTEditObjSelect.setCondition(
					h.group(h.opAnd()).addCondition(
							new EIMSearchConditionIn(h.opAnd(),EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID,h.opIn(),sql))
						);

			noSTEditObjSelect.setResultAttrs(new ArrayList<>());
			EIMSearchResultList noSTPublicObjList = SearchUtils.searchObjects(sess, noSTEditObjSelect, new EIMSearchLimitCountCondition(-1, false));

			for(int i = 0; i< noSTPublicObjList.size() ;i ++){
				noSTPublicObjSet.add((long) ((EIMObject) noSTPublicObjList.get(i)).getId());
			}
		}

		/**
		 * ステータスなし公開ドキュメントIDのSetを取得します。
		 * @return ステータスなしドキュメントIDのSet
		 */
		public Set<Long> getNoStatusPublicObjectSet() {
			return noSTPublicObjSet;
		}

	}

	/**
	 * ドキュメントインデックス生成時に必要な、ドキュメントが保持するファイル情報とステータスなし公開ドキュメント判定情報
	 * を一括取得し返却します。
	 * @see jp.co.ctc_g.eim.search.solr.indexBase.business.service.plugin.impl.FileIndexAdditionalInfoPlugInImpl#getAdditionalInfo(java.util.List)
	 */
	@Override
	public Object getAdditionalInfo(List<EIMObject> objectList) throws Exception {

		// 親クラスを呼び出し、ファイル情報を取得
		DocumentIndexAdditionalInfo additionalInfo = new DocumentIndexAdditionalInfo();
		additionalInfo.init(objectList);

		// ステータスなし公開ドキュメントを取得
		additionalInfo.initNoStatusPulicObject(objectList);

		return additionalInfo;
	}

}
