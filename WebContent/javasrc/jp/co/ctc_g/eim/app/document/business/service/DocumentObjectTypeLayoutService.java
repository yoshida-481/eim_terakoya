package jp.co.ctc_g.eim.app.document.business.service;

import jp.co.ctc_g.eim.app.form.business.domain.ObjectTypeLayoutDomain;

/**
 * レイアウト情報を保持したオブジェクトタイプに関する操作を行うビジネスサービスです。
 * @since Ver6.6
 */
public interface DocumentObjectTypeLayoutService {

	/**
	 * オブジェクトタイプに割り当てられた属性タイプの引継ぎ、関連付け設定をします。<br>
	 * 
	 * @param objectTypeLayout レイアウト情報を保持したオブジェクトタイプ<br>
	 * <br>
	 * <p style="padding-left:4em">objectTypeLayout内のレイアウト情報は以下のように処理されます。<br>
	 * (下記のレイアウト情報以外の情報は、全て編集できません。)<br>
	 * <table width="100%" border="1">
	 *  <tr bgcolor="#EEEEFF">
	 *    <th width="100">フィールド名</th>
	 *    <th width="70">必須</th>
	 *    <th width="70">設定可</th>
	 *    <th width="300">制約</th>
	 *    <th>備考</th>
	 *  </tr>
	 *  <tr>
	 *    <td>id</td>
	 *    <td align="center">○</td>
	 *    <td align="center">-</td>
	 *    <td align="center">-</td>
	 *    <td>本メソッドでは編集できません。</td>
	 *  </tr>
	 *  <tr>
	 *    <td>attributeLayoutList</td>
	 *    <td align="center">○</td>
	 *    <td align="center">○</td>
	 *    <td align="center">-</td>
	 *    <td>
	 *    属性タイプの引継ぎ、関連付け設定をします。<br>
	 *    リスト内に存在する属性タイプだけが、引継ぎ、関連付け設定の対象となります。<br>
	 *    ※自動生成されたオブジェクトタイプオブジェクトの属性値に登録されます。
	 *    </td>
	 *  </tr>
	 * </table>
	 * <br>
	 * @throws Exception 以下の例外を通知します。
	 * <p style="padding-left:4em">
	 * <table width="100%" border="1">
	 *  <tr bgcolor="#EEEEFF">
	 *    <th width="100">エラーコード</th>
	 *    <th>原因、処置</th>
	 *  </tr>
	 *  <tr>
	 *    <td>EIM.ERROR.LOGIC.OBJECTTYPELAYOUT.VALUE.ILLEGAL</td>
	 *    <td>引数objectTypeLayoutとしてnullが入力されました。</td>
	 *  </tr>
	 *  <tr>
	 *    <td>EIM.ERROR.LOGIC.OBJECTTYPELAYOUT.NOTFOUND</td>
	 *    <td>対象のオブジェクトタイプが存在しません。</td>
	 *  </tr>
	 *  <tr>
	 *    <td>EIM.ERROR.LOGIC.ATTTYPE.RELATION.NOTFOUND</td>
	 *    <td>指定した属性タイプは適用されていません。</td>
	 *  </tr>
	 * </table>
	 * <br>
	 * @since Ver6.6
	 * <br>
	 */
	public void inheritanceAndRelationAttributeType(ObjectTypeLayoutDomain objectTypeLayout) throws Exception;
}
