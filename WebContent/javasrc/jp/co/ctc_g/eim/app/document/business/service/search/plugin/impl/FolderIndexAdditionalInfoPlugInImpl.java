package jp.co.ctc_g.eim.app.document.business.service.search.plugin.impl;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import eim.bo.EIMObject;
import jp.co.ctc_g.eim.framework2.business.dao.RelationDao;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RelationDomain;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionIn;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchLimitCountCondition;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectRelation;
import jp.co.ctc_g.eim.search.solr.indexBase.business.service.plugin.IndexAdditionalInfoPlugIn;

/**
 * フォルダインデックス生成時に必要な、フォルダ内のコンテンツ存在判定情報の一括取得を行うプラグインクラスです。
 */
public class FolderIndexAdditionalInfoPlugInImpl implements IndexAdditionalInfoPlugIn {

	/** リレーションDao */
	RelationDao relationDao = null;

	/**
	 * 空フォルダフラグを設定するためにリレーションを一括取得し、フォルダ内のコンテンツが存在するフォルダIDのSetを返却します。
	 * @see jp.co.ctc_g.eim.search.solr.indexBase.business.service.plugin.IndexAdditionalInfoPlugIn#getAdditionalInfo(java.util.List)
	 */
	@Override
	public Object getAdditionalInfo(List<EIMObject> objectList) throws Exception {

		// 登録先フォルダの配下のオブジェクト取得
		SearchSelectObject.SearchConditionBuildHelper h = new SearchSelectObject.SearchConditionBuildHelper();
		SearchSelectRelation select = new SearchSelectRelation();
		SearchSelectObject parent = new SearchSelectObject();
		SearchSelectObject child = new SearchSelectObject();
		SearchLimitCountCondition limit = new SearchLimitCountCondition(SearchLimitCountCondition.UNLIMITED, false);

		// フォルダのID配列
		Set<Long> idSet = objectList.stream().map(obj -> (long) obj.getId()).collect(Collectors.toSet());
		long ids[] = idSet.stream().mapToLong(id -> Long.valueOf(id)).toArray();

		select.setCondition(
			h.group(
				h.opAnd()
			).addCondition(
				new SearchConditionIn(
					h.opAnd(),
					SearchSelectRelation.PsedoAttributeTypeEnum.PARENT,
					h.opIn(),
					ids
				)
			)
		);

		select.setResultAttrs(Collections.emptyList());
		parent.setResultAttrs(Collections.emptyList());
		child.setResultAttrs(Collections.emptyList());

		// リレーション取得
		List<RelationDomain> relations = relationDao.getList(select, parent, child, limit);

		// 取得できた親フォルダIDを保管
		Set<Long> notEmptyFolderIdSet = relations.stream().map(relation -> relation.getParent().getId()).collect(Collectors.toSet());

		return notEmptyFolderIdSet;
	}

	/**
	 * リレーションDaoを取得します。
	 * @return リレーションDao
	 */
	public RelationDao getRelationDao() {
		return relationDao;
	}

	/**
	 * リレーションDaoを設定します。
	 * @param relationDao リレーションDao
	 */
	public void setRelationDao(RelationDao relationDao) {
		this.relationDao = relationDao;
	}

}
