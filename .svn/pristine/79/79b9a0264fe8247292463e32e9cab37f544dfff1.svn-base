このフォルダにはSolrのCoreの設定ファイルが保管されています。

[1] Coreの作成方法

coresディレクトリ内のEIMOBJ、EIMKWD、EIMPAGEディレクトリを「/home/solr/solr-9.6.1/server/solr/cofigsets」ディレクトリ配下に配置します。
以下のコマンドを実行してCoreを作成します。

$ solr create_core -c {コア名称} -d {コア設定ファイル保管ディレクトリ}

例)
$ solr create_core -c EIMOBJ -d EIMOBJ

参考.Colreの削除

$ solr delete -c {コア名称}

[2] solrconfigの設定変更箇所

Solr(V9.6.1)の標準solrconfigに対する設定変更箇所について説明します。

(1) 各コア共通

a) デフォルト検索フィールド"_text_"の指定を削除

  <!-- Shared parameters for multiple Request Handlers -->
  <initParams path="/update/**,/query,/select,/spell">
    <lst name="defaults">
    </lst>
  </initParams>

b) 自動フィールド生成(update.autoCreateFields)のfalseに変更

  <!-- The update.autoCreateFields property can be turned to false to disable schemaless mode -->
  <updateRequestProcessorChain name="add-unknown-fields-to-the-schema" default="${update.autoCreateFields:false}"

※ 下記のコマンドで設定ができるはずだが、おそらくスクリプトの不備によって404エラーとなってしまう。コマンドを実行しなくていいように設定ファイルを修正しました。

solr config -c EIMOBJ -p 8983 -action set-user-property -property update.autoCreateFields -value false

なお、-solrUrlを指定することで実行できます。

solr config -c EIMOBJ -p 8983 -action set-user-property -property update.autoCreateFields -value false -solrUrl http://localhost:8983/solr

(1) EIMOBJ

a) テキスト抽出処理、テキスト解析処理を行うライブラリを追加

  <!-- for Text Extraction -->
  <lib dir="${solr.install.dir:../../../..}/modules/extraction/lib" regex=".*\.jar" />

  <!-- for Text Filter -->
  <lib dir="${solr.install.dir:../../../..}/modules/analysis-extras/lib" regex=".*\.jar" />

b) テキスト抽出リクエストヘッダの追加 

  <!-- Solr Cell Update Request Handler

       http://wiki.apache.org/solr/ExtractingRequestHandler
    -->
  <requestHandler name="/update/extract"
                  startup="lazy"
                  class="solr.extraction.ExtractingRequestHandler" >
    <lst name="defaults">
      <str name="lowernames">true</str>
      <str name="uprefix">ignored_</str>
    </lst>
    <!-- Tika configuration -->
    <str name="parseContext.config">parseContext.xml</str>
  </requestHandler>

c) parseContext.xmlを配置してリクエストヘッダb)の設定が参照するTika configurationでZipファイル内保管ファイルの文字コードをMS932に設定

d) スニペット抽出方式をoriginal(デフォルト)からunified(推奨)に変更

      <!-- Configuation of highlinghting for EIMANAGER. -->
      <str name="hl.method">unified</str>
      <int name="hl.maxAnalyzedChars">${eim.hl.maxAnalyzedChars}</int>
      <str name="hl.bs.type">CHARACTER</str>

e) solrcore.propertiesを配置してカスタム変数d)の設定が参照するeim.hl.maxAnalyzedCharsを定義

#
# EIMANAGER custom variables.
#

# The character limit to look for highlights, after which no highlighting will be done.
# This value is referenced from "hl.maxAnalyzedChars" of Search Component and "maxChars" of the snippet extraction target field.
# Solr default value is 51200. Increasing this will affect memory usage and response time.
eim.hl.maxAnalyzedChars=51200

(1) EIMPAGE

a) テキスト解析処理を行うライブラリを追加

  <!-- for Text Filter -->
  <lib dir="${solr.install.dir:../../../..}/modules/analysis-extras/lib" regex=".*\.jar" />

[3] managed-schemaの設定変更箇所

Solr(V9.7.0)の標準managed-schemaに対する設定変更箇所について説明します。

(1) 各Core共通

a) _version_を除いて、下記デフォルトfield定義を削除

    <field name="id" type="string" indexed="true" stored="true" required="true" multiValued="false" />
    <field name="_root_" type="string" indexed="true" stored="false" />
    <field name="_nest_path_" type="_nest_path_" /><fieldType name="_nest_path_" class="solr.NestPathField" />
    <field name="_text_" type="text_general" indexed="true" stored="false" multiValued="true"/>

b) ignored_*を除いて(各言語固有の定義も除いて)、下記デフォルトdynamic field定義を削除

    <dynamicField name="*_i"   type="pint"     indexed="true"  stored="true"/>
    <dynamicField name="*_is"  type="pints"    indexed="true"  stored="true"/>
    <dynamicField name="*_s"   type="string"   indexed="true"  stored="true" />
    <dynamicField name="*_ss"  type="strings"  indexed="true"  stored="true"/>
    <dynamicField name="*_l"   type="plong"    indexed="true"  stored="true"/>
    <dynamicField name="*_ls"  type="plongs"   indexed="true"  stored="true"/>
    <dynamicField name="*_b"   type="boolean"  indexed="true"  stored="true"/>
    <dynamicField name="*_bs"  type="booleans" indexed="true"  stored="true"/>
    <dynamicField name="*_f"   type="pfloat"   indexed="true"  stored="true"/>
    <dynamicField name="*_fs"  type="pfloats"  indexed="true"  stored="true"/>
    <dynamicField name="*_d"   type="pdouble"  indexed="true"  stored="true"/>
    <dynamicField name="*_ds"  type="pdoubles" indexed="true"  stored="true"/>
    <dynamicField name="*_dt"  type="pdate"    indexed="true"  stored="true"/>
    <dynamicField name="*_dts" type="pdates"   indexed="true"  stored="true"/>
    <dynamicField name="*_t"   type="text_general" indexed="true" stored="true" multiValued="false"/>
    <dynamicField name="*_txt" type="text_general" indexed="true" stored="true"/>
    <dynamicField name="random_*" type="random"/>
    <dynamicField name="*_str" type="strings" stored="false" docValues="true" indexed="false" useDocValuesAsStored="false"/>
    <dynamicField name="*_p"  type="location" indexed="true" stored="true"/>
    <dynamicField name="*_srpt"  type="location_rpt" indexed="true" stored="true"/>
    <dynamicField name="*_dpf" type="delimited_payloads_float" indexed="true"  stored="true"/>
    <dynamicField name="*_dpi" type="delimited_payloads_int" indexed="true"  stored="true"/>
    <dynamicField name="*_dps" type="delimited_payloads_string" indexed="true"  stored="true"/>
    <dynamicField name="attr_*" type="text_general" indexed="true" stored="true" multiValued="true"/>

c) uniqueKeyをidからKEYに変更

    <!-- Field to use to determine and enforce document uniqueness.
      Unless this field is marked with required="false", it will be a required field
    -->
    <uniqueKey>KEY</uniqueKey>

(2) EIMOBJ

a) EIMANAGER用カスタムfieldType、field、dynamic field、copy fieldを追加

(3) EIMKWD

a) EIMANAGER用カスタムfieldType、field、copy fieldを追加

(4) EIMPAGE

a) EIMANAGER用カスタムfieldType、field、copy fieldを追加

