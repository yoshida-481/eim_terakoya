package common.tools.internal;

import java.sql.ResultSet;
import java.sql.Statement;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMObject;
import eim.bo.EIMValueType;
import eim.net.EIMSession;
import eim.util.SqlUtils;
import eim.util.TypeConvertUtils;
import eim.util.internal.search.cache.InstanceCacheForEIMAttributeType;
import eim.util.internal.search.sql.SearchJavaUtil;
import eim.util.internal.search.sql.SearchSqlUtil;
/**
 * オブジェクト属性を読み込んでセットするクラス。
 *
 */
class AttributeLoader {
	
	/**
	 * ロガー
	 */
	protected Log log = LogFactory.getLog(this.getClass());

	/**
	 * 処理対象EIMObjectのリスト
	 */
	protected List _objs;

	/**
	 * EIMObjectのIDとリスト内インデックスとのマッピング
	 */
	protected Map _idToIndexMap = new Hashtable();

	/**
	 * 返却EIMObjectが使用中の属性タイプ
	 */
	protected Set _usingAttrTypes = new HashSet();

	/**
	 * 返却する結果セット
	 */
	protected List _resultAttrTypes = new ArrayList();

	/**
	 * コンストラクタ
	 *
	 * @param parentBuilder 親ビルダー
	 */
	AttributeLoader() {
	}

	/**
	 * 結果データが使用している属性タイプのセットに引数IDを追加する。
	 *
	 * @param attrTypeId 属性タイプID
	 * @return 引数値と同じ値
	 */
	private long addUsingAttrTypes(long attrTypeId)
	{
		_usingAttrTypes.add(new Long(attrTypeId));
		return attrTypeId;
	}

	/**
	 * リスト内のEIMObjectに属性をセットする
	 *
	 * @param sess セッション
	 * @param objs EIMObjectのリスト
	 * @throws Exception 予期せぬ例外
	 */
	void loadAttrToObj(EIMSession sess, List<EIMObject> objs) throws Exception
	{
		loadRsAttrToObj(sess, objs);
		replaceToRealEIMAttributes(sess, objs);
	}

	/**
	 * リスト内のEIMObjectに暫定属性RsEIMAttributeをセットする
	 * @param sess セッション
	 * @param objs EIMObjectのリスト
	 * @throws Exception 予期せぬ例外
	 */
	private void loadRsAttrToObj(EIMSession sess, List<EIMObject> objs) throws Exception
	{
		_objs = objs;
		// EIMObjectのIDとリストINDEXとのマッピングを作成
		createIdToIndexMap();

		String sql = createAttrSelectSql(sess, Arrays.asList(_idToIndexMap.keySet().toArray()));
		if (sql == null)
			return;
		Statement stmt = SearchSqlUtil.getStatement(sess);
		try
		{
			log.trace(sql);

			ResultSet rs = stmt.executeQuery(sql);

			try
			{
				while (rs.next())
				{
					RsEIMAttribute attr = loadRsAttribute(rs);
					Long id = new Long(rs.getLong("id"));
					Integer index = (Integer) _idToIndexMap.get(id);
					if (index == null)
						continue;
					EIMObject obj = (EIMObject) objs.get(index.intValue());
					if (obj == null)
						continue;
					obj.getAttributeList().add(attr);
				}
			} finally
			{
				rs.close();
			}
		} finally
		{
			stmt.close();
		}
	}

	/**
	 * EIMObjectのIDとリストINDEXとのマッピングを作成する
	 *
	 */
	private void createIdToIndexMap()
	{
		for (int i = 0; i < _objs.size(); i++)
		{
			EIMObject obj = (EIMObject) _objs.get(i);
			_idToIndexMap.put(new Long(obj.getId()), new Integer(i));
		}
	}

	/**
	 * 属性をSelectするSQL文を生成して返す
	 *
	 * @param ids EIMObjectのIDリスト
	 * @return SQL文
	 */
	private String createAttrSelectSql(EIMSession sess, List ids)
		throws Exception
	{
		List typesInt = null;
		List typesDate = null;
		List typesStr = null;
		List typesText = null;
		List typesDouble = null;
		Boolean multipleFlg = null;

		//if (_parentBuilder.doLimitResultAttrs())
		//{
			multipleFlg = false;

			typesInt = new ArrayList();
			typesDate = new ArrayList();
			typesStr = new ArrayList();
			typesText = new ArrayList();
			typesDouble = new ArrayList();
			for (Iterator i = _resultAttrTypes.iterator(); i
					.hasNext();)
			{
				EIMAttributeType attrType = (EIMAttributeType) i.next();
				if (attrType.getClass() != EIMAttributeType.class)
					continue;

				if(attrType.isMultiple())
					multipleFlg = true;

				Long typeId = new Long(attrType.getId());
				switch (attrType.getValueType().getId()) {
				case EIMValueType.INTEGER:
					typesInt.add(typeId);
					break;
				case EIMValueType.DATE:
					typesDate.add(typeId);
					break;
				case EIMValueType.STRING:
					typesStr.add(typeId);
					break;
				case EIMValueType.TEXT:
					typesText.add(typeId);
					break;
				case EIMValueType.DOUBLE:
					typesDouble.add(typeId);
					break;
				}
			}
		//}
		StringBuffer unionSb = new StringBuffer();
		appendAttrUnionSql(unionSb, EIMValueType.INTEGER, typesInt);
		appendAttrUnionSql(unionSb, EIMValueType.DATE, typesDate);
		appendAttrUnionSql(unionSb, EIMValueType.STRING, typesStr);
		appendAttrUnionSql(unionSb, EIMValueType.TEXT, typesText);
		appendAttrUnionSql(unionSb, EIMValueType.DOUBLE, typesDouble);
		if (unionSb.length() == 0)
			return null;

		unionSb.insert(0, "select valType, id , type, val_int, val_str, val_date, val_text, key, val_double from (");
		unionSb.append(") EOA where ");

		//複数属性値フラグ未取得なら取得
		if(multipleFlg == null)
			multipleFlg = getMultipleFlg(sess);

		//条件句を追加
		SearchSqlUtil.expandListToInUnionALL(unionSb, ids, "id");

		//複数値属性ありならorder by句を付与。
		if(multipleFlg)
			unionSb.append(" order by key ");

		return unionSb.toString();
	}

	/**
	 * 各種属性テーブル(OBJINT,OBJDATE,OBJST,OBJTEXT,OBJREAL )に対するUNION SQLを追加する
	 *
	 * @param unionSb 文字列バッファ
	 * @param valueTypeId 値タイプID
	 * @param types 属性タイプ値のリスト
	 */
	private void appendAttrUnionSql(StringBuffer unionSb, int valueTypeId, List types)
	{
		if ( types.size() == 0)
			return;

		String valInt = null;
		String valDate = null;
		String valStr = null;
		String valText = null;
		String valDouble = null;
		String tblName = null;
		switch (valueTypeId) {
		case EIMValueType.INTEGER:
			valInt = "value";
			valDouble = "null";
			valDate = "null";
			valStr = "null";
			valText = "null";
			tblName = "EIMOBJINT";
			break;
		case EIMValueType.DATE:
			valInt = "null";
			valDouble = "null";
			valDate = "value";
			valStr = "null";
			valText = "null";
			tblName = "EIMOBJDATE";
			break;
		case EIMValueType.STRING:
			valInt = "null";
			valDouble = "null";
			valDate = "null";
			valStr = "value";
			valText = "null";
			tblName = "EIMOBJSTR";
			break;
		case EIMValueType.TEXT:
			valInt = "null";
			valDouble = "null";
			valDate = "null";
			valStr = "null";
			valText = "value";
			tblName = "EIMOBJTEXT";
			break;
		case EIMValueType.DOUBLE:
			valInt = "null";
			valDouble = "value";
			valDate = "null";
			valStr = "null)";
			valText = "null";
			tblName = "EIMOBJREAL";
			break;
		}
		if (unionSb.length() > 0)
			unionSb.append(" union all");
		unionSb.append(" select ")//
				.append(valueTypeId).append(" as valType")//
				.append(",id , type , ")//
				.append(valInt).append(" as val_int , ")//
				.append(valDate).append(" as val_date , ")//
				.append(valText).append(" as val_text , ")//
				.append(valStr).append(" as val_str, key, ")//
				.append(valDouble).append(" as val_double from ")//
				.append(tblName);
			unionSb.append(" where ");
			SearchSqlUtil.expandListToIn(unionSb, types, "type");
	}

	/**
	 * リザルトセットからデータを読み込みRsEIMAttributeインスタンスを生成する
	 *
	 * @param rs リザルトセット
	 * @return RsEIMAttributeインスタンス
	 * @throws Exception 予期せぬ例外
	 */
	private RsEIMAttribute loadRsAttribute(ResultSet rs) throws Exception
	{
		long type = addUsingAttrTypes(rs.getLong("type"));
		int valType = rs.getInt("valType");
		long intValue = 0;
		double doubleValue = 0;
		String strValue = null;
		Timestamp dateValue = null;
		String textValue = null;
		switch (valType) {
		case EIMValueType.INTEGER:
			intValue = rs.getLong("val_int");
			break;
		case EIMValueType.DATE:
			dateValue = rs.getTimestamp("val_date");
			break;
		case EIMValueType.STRING:
			strValue = rs.getString("val_str");
			break;
		case EIMValueType.TEXT:
			textValue = SqlUtils.getStringFromCLOB(rs, "val_text");
			break;
		case EIMValueType.DOUBLE:
			doubleValue = rs.getDouble("val_double");
			break;
		}
		return new RsEIMAttribute(type, intValue, strValue, dateValue, textValue, doubleValue);
	}

	/**
	 * リスト内のEIMObjectにセットされているRsEIMAttributeインスタンスをEIMAttributeインスタンスに置き換える
	 *
	 * @param sess セッション
	 * @param objs EIMObjectリスト
	 * @throws Exception 予期せぬ例外
	 */
	private void replaceToRealEIMAttributes(EIMSession sess, List objs)
			throws Exception
	{
		// 必要な属性タイプをキャッシュする
		InstanceCacheForEIMAttributeType attrTypeCache = new InstanceCacheForEIMAttributeType(sess,
				_usingAttrTypes);

		// オブジェクトごとに属性設定
		for (Iterator i = objs.iterator(); i.hasNext();)
		{
			EIMObject obj = (EIMObject) i.next();
			List attrs = obj.getAttributeList();

			// 属性値ハッシュマップの生成
			// [key]属性タイプ、[value]属性値が設定されたRsEIMAttributeのArrayList
			HashMap attrValueMap = new HashMap();
			List attrTypeList = new ArrayList();
			for (int j = 0; j < attrs.size(); j++)
			{
				RsEIMAttribute rsAttr = (RsEIMAttribute) attrs.get(j);
				// 同じタイプの属性値が既に設定済みの場合
				if(attrValueMap.containsKey(new Long(rsAttr._attrType)))
				{
					// RsEIMAttributeのリストに追加
					List rsAttrList = (ArrayList)attrValueMap.get(new Long(rsAttr._attrType));
					rsAttrList.add(rsAttr);
					attrValueMap.put(new Long(rsAttr._attrType), rsAttrList);
				}
				else
				{
					// 新規属性タイプのRsEIMAttributeのリストを生成
					List rsAttrList = new ArrayList();
					rsAttrList.add(rsAttr);
					attrValueMap.put(new Long(rsAttr._attrType), rsAttrList);

					// 属性タイプのリストに追加
					attrTypeList.add(new Long(rsAttr._attrType));
				}
			}

			// オブジェクトの属性リストの初期化
			attrs.clear();

			// 属性タイプごとに、EIMAttributeの作成
			for(int k = 0; k < attrTypeList.size(); k++)
			{
				// 属性値リストの取得
				List rsAttrList = (ArrayList)attrValueMap.get((Long)attrTypeList.get(k));

				// 属性値リストの先頭データからEIMAttributeType生成
				RsEIMAttribute rsAttr = (RsEIMAttribute)rsAttrList.get(0);
				EIMAttributeType attrType = (EIMAttributeType) attrTypeCache.get(rsAttr._attrType);
				EIMAttribute attr = null;

				// 数値型属性の設定
				if(attrType.getValueType().getId() == EIMValueType.INTEGER)
				{
					// 属性値をint型の配列につみなおす
					long[] intValueList = new long[rsAttrList.size()];
					for(int l = 0; l < rsAttrList.size(); l++)
					{
						RsEIMAttribute rsAttrTemp = (RsEIMAttribute)rsAttrList.get(l);
						intValueList[l] = rsAttrTemp._intValue;
					}
					// EIMAttributeの生成
					attr = new EIMAttribute(attrType, TypeConvertUtils.convertToBuildTypeArray(intValueList), null, null, null);
				}
				// 文字列型属性の設定
				else if(attrType.getValueType().getId() == EIMValueType.STRING)
				{
					// 属性値をString型の配列につみなおす
					String[] strValueList = new String[rsAttrList.size()];
					for(int l = 0; l < rsAttrList.size(); l++)
					{
						RsEIMAttribute rsAttrTemp = (RsEIMAttribute)rsAttrList.get(l);
						strValueList[l] = rsAttrTemp._strValue;
					}
					// EIMAttributeの生成
					attr = new EIMAttribute(attrType, null, strValueList, null, null);
				}
				// 日付型属性の設定
				else if(attrType.getValueType().getId() == EIMValueType.DATE)
				{
					// 属性値をjava.sql.Date型の配列につみなおす
					java.sql.Date[] dateValueList = new java.sql.Date[rsAttrList.size()];
					for(int l = 0; l < rsAttrList.size(); l++)
					{
						RsEIMAttribute rsAttrTemp = (RsEIMAttribute)rsAttrList.get(l);
						dateValueList[l] = SearchJavaUtil.convToSqlDate(rsAttrTemp._dateValue);
					}
					// EIMAttributeの生成
					attr = new EIMAttribute(attrType, null, null, dateValueList, null);
				}
				// テキスト型属性の設定
				else if(attrType.getValueType().getId() == EIMValueType.TEXT)
				{
					// 属性値をString型の配列につみなおす
					String[] textValueList = new String[rsAttrList.size()];
					for(int l = 0; l < rsAttrList.size(); l++)
					{
						RsEIMAttribute rsAttrTemp = (RsEIMAttribute)rsAttrList.get(l);
						textValueList[l] = rsAttrTemp._textValue;
					}
					// EIMAttributeの生成
					attr = new EIMAttribute(attrType, null, null, null, textValueList);
				}
				// ダブル型属性の設定
				else if(attrType.getValueType().getId() == EIMValueType.DOUBLE)
				{
					// 属性値をdouble型の配列につみなおす
					double[] dValueList = new double[rsAttrList.size()];
					for(int l = 0; l < rsAttrList.size(); l++)
					{
						RsEIMAttribute rsAttrTemp = (RsEIMAttribute)rsAttrList.get(l);
						dValueList[l] = rsAttrTemp._doubleValue;
					}
					// EIMAttributeの生成
					attr = new EIMAttribute(attrType, dValueList);
				}
				// オブジェクトの属性リストに追加
				attrs.add(attr);
			}
		}
	}

	/*
	 * 複数値をとる属性があるかどうかを取得します。
	 */
	private boolean getMultipleFlg(EIMSession sess) throws Exception
	{
		boolean multipleFlg = false;

		String sql = "select count(*) cnt from eimattr ATTR inner join " +
					"(eimattrtype ATTRTYPE inner join eimobjtype OBJTYPE on OBJTYPE.id=ATTRTYPE.type) " +
					"on ATTR.id=ATTRTYPE.id " +
					"where ATTR.is_multiple <> 0";

		Statement stmt = SearchSqlUtil.getStatement(sess);

		try
		{
			ResultSet rs = stmt.executeQuery(sql);

			try
			{
				if (rs.next())
					multipleFlg = rs.getInt("cnt") > 0 ? true : false;
			}
			finally
			{
				rs.close();
			}
		}
		finally
		{
			stmt.close();
		}

		return multipleFlg;
	}
}
