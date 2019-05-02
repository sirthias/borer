/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.benchmarks

import org.openjdk.jmh.annotations._
import spray.json._
import java.nio.charset.StandardCharsets.UTF_8

import io.bullet.borer.{Default, Nullable}

object SprayCodecs {
  import spray.json.DefaultJsonProtocol._

  implicit val fooFormat  = jsonFormat5(Foo.apply)
  implicit val foosFormat = implicitly[RootJsonFormat[Map[String, Foo]]]
  implicit val intsFormat = implicitly[RootJsonFormat[List[Int]]]
}

import SprayCodecs._

class SprayEncodingBenchmark extends EncodingBenchmark {

  @Benchmark
  def encodeFoos: Array[Byte] = foosFormat.write(foos).compactPrint.getBytes(UTF_8)

  @Benchmark
  def encodeInts: Array[Byte] = intsFormat.write(ints).compactPrint.getBytes(UTF_8)

  @Benchmark
  def encodeEmptyArray: Array[Byte] = intsFormat.write(Nil).compactPrint.getBytes(UTF_8)
}

class SprayDecodingBenchmark extends DecodingBenchmark {

  @Benchmark
  def decodeFoos: Map[String, Foo] = JsonParser(new String(foosJson, UTF_8)).convertTo[Map[String, Foo]]

  @Benchmark
  def decodeInts: List[Int] = JsonParser(new String(intsJson, UTF_8)).convertTo[List[Int]]

  @Benchmark
  def decodeEmptyArray: List[Int] = JsonParser(new String(emptyArrayJson, UTF_8)).convertTo[List[Int]]
}

class SprayDomBenchmark extends DomBenchmark {

  private var root: JsValue = _
  def setup(): Unit         = root = JsonParser(new String(fileBytes, UTF_8))

  @Benchmark
  def encodeDom: Array[Byte] = root.compactPrint.getBytes(UTF_8)

  @Benchmark
  def decodeDom: JsValue = JsonParser(new String(fileBytes, UTF_8))
}

class SprayModelBenchmark extends DomBenchmark {

  private var root: Product = _

  implicit lazy val format: RootJsonFormat[Product] = {
    import DefaultJsonProtocol._

    implicit def nullableJsonFormat[T: JsonFormat: Default]: JsonFormat[Nullable[T]] =
      new JsonFormat[Nullable[T]] {
        def write(value: Nullable[T])      = value.value.toJson
        def read(value: JsValue) = if (value == JsNull) Default.get[T] else value.convertTo[T]
      }

    val c = fileName match {
      case "australia-abc.json" ⇒
        implicit val a = jsonFormat2(Australia.Geometry)
        implicit val b = jsonFormat6(Australia.Properties)
        implicit val c = jsonFormat1(Australia.Properties1)
        implicit val d = jsonFormat5(Australia.Features)
        implicit val e = jsonFormat2(Australia.Crs)
        jsonFormat4(Australia.RootInterface)

      case "bitcoin.json" ⇒
        implicit val a = jsonFormat2(Bitcoin.SpendingOutpoints)
        implicit val b = jsonFormat8(Bitcoin.PrevOut)
        implicit val c = jsonFormat4(Bitcoin.Inputs)
        implicit val d = jsonFormat7(Bitcoin.Out)
        implicit val e = jsonFormat13(Bitcoin.Txs)
        jsonFormat1(Bitcoin.RootInterface)

      case "doj-blog.json" ⇒
        implicit val a = jsonFormat2(DojBlog.ResponseInfo)
        implicit val b = jsonFormat3(DojBlog.Resultset)
        implicit val c = jsonFormat3(DojBlog.Metadata)
        implicit val d = jsonFormat2(DojBlog.Component)
        implicit val e = jsonFormat13(DojBlog.Results)
        jsonFormat2(DojBlog.RootInterface)

      case "eu-lobby-country.json" ⇒
        implicit val a = jsonFormat0(EuLobbyCountry.Facets)
        implicit val b = jsonFormat6(EuLobbyCountry.Results)
        jsonFormat7(EuLobbyCountry.RootInterface)

      case "eu-lobby-financial.json" ⇒
        implicit val a = jsonFormat0(EuLobbyFinancial.Facets)
        implicit val b = jsonFormat8(EuLobbyFinancial.CustomIncomes)
        implicit val c = new JsonFormat[EuLobbyFinancial.Results] {
          def write(obj: EuLobbyFinancial.Results) = ???
          def read(json: JsValue) = {
            val x = json.asJsObject
            EuLobbyFinancial.Results(
              x.fields("other_sources_donation").convertTo[Nullable[Int]],
              x.fields("status").convertTo[String],
              x.fields("turnover_min").convertTo[Nullable[Int]],
              x.fields("eur_sources_procurement").convertTo[Int],
              x.fields("end_date").convertTo[String],
              x.fields("eur_sources_procurement_src").convertTo[Nullable[String]],
              x.fields("new_organisation").convertTo[Nullable[String]],
              x.fields("turnover_max").convertTo[Nullable[Int]],
              x.fields("updated_at").convertTo[String],
              x.fields("cost_min").convertTo[Nullable[Int]],
              x.fields("direct_rep_costs_max").convertTo[Nullable[Int]],
              x.fields("representative").convertTo[String],
              x.fields("cost_absolute").convertTo[Nullable[Int]],
              x.fields("eur_sources_grants_src").convertTo[Nullable[String]],
              x.fields("id").convertTo[String],
              x.fields("customIncomes").convertTo[Seq[EuLobbyFinancial.CustomIncomes]],
              x.fields("total_budget").convertTo[Nullable[Int]],
              x.fields("turnover_absolute").convertTo[Nullable[Int]],
              x.fields("eur_sources_grants").convertTo[Int],
              x.fields("other_sources_contributions").convertTo[Nullable[Int]],
              x.fields("cost_max").convertTo[Nullable[Int]],
              x.fields("created_at").convertTo[String],
              x.fields("uri").convertTo[String],
              x.fields("public_financing_infranational").convertTo[Nullable[Int]],
              x.fields("direct_rep_costs_min").convertTo[Nullable[Int]],
              x.fields("public_financing_total").convertTo[Nullable[Int]],
              x.fields("other_sources_total").convertTo[Nullable[Int]],
              x.fields("other_financial_information").convertTo[Nullable[String]],
              x.fields("public_financing_national").convertTo[Nullable[Int]],
              x.fields("no_clients").convertTo[Nullable[String]],
              x.fields("type").convertTo[String],
              x.fields("start_date").convertTo[String],
            )
          }
        }
        jsonFormat7(EuLobbyFinancial.RootInterface)

      case "eu-lobby-repr.json" ⇒
        implicit val a = jsonFormat0(EuLobbyRepr.Facets)
        implicit val b = new JsonFormat[EuLobbyRepr.Results] {
          def write(obj: EuLobbyRepr.Results) = ???
          def read(json: JsValue) = {
            val x = json.asJsObject
            EuLobbyRepr.Results(
              x.fields("activity_consult_committees").convertTo[Nullable[String]],
              x.fields("activity_high_level_groups").convertTo[Nullable[String]],
              x.fields("head_office_lat").convertTo[Nullable[Double]],
              x.fields("updated_at").convertTo[String],
              x.fields("entity").convertTo[String],
              x.fields("number_of_natural_persons").convertTo[Nullable[Int]],
              x.fields("legal").convertTo[String],
              x.fields("native_name").convertTo[Nullable[String]],
              x.fields("head_office_country").convertTo[String],
              x.fields("id").convertTo[String],
              x.fields("activity_industry_forums").convertTo[String],
              x.fields("contact_country").convertTo[Int],
              x.fields("head_office_postbox").convertTo[Nullable[String]],
              x.fields("networking").convertTo[Nullable[String]],
              x.fields("members_75").convertTo[Nullable[Int]],
              x.fields("main_category").convertTo[Int],
              x.fields("members_50").convertTo[Nullable[Int]],
              x.fields("activity_expert_groups").convertTo[String],
              x.fields("sub_category_title").convertTo[String],
              x.fields("other_code_of_conduct").convertTo[Nullable[String]],
              x.fields("head_office_town").convertTo[String],
              x.fields("info_members").convertTo[String],
              x.fields("head").convertTo[String],
              x.fields("status").convertTo[String],
              x.fields("main_category_title").convertTo[String],
              x.fields("head_office_street").convertTo[String],
              x.fields("activity_inter_groups").convertTo[String],
              x.fields("acronym").convertTo[Nullable[String]],
              x.fields("activity_eu_legislative").convertTo[String],
              x.fields("registration_date").convertTo[String],
              x.fields("activity_relevant_comm").convertTo[String],
              x.fields("head_office_post_code").convertTo[Nullable[String]],
              x.fields("goals").convertTo[String],
              x.fields("members").convertTo[Int],
              x.fields("last_update_date").convertTo[String],
              x.fields("members_fte").convertTo[Double],
              x.fields("head_office_phone").convertTo[String],
              x.fields("members_25").convertTo[Nullable[Int]],
              x.fields("web_site_url").convertTo[Nullable[String]],
              x.fields("sub_category").convertTo[Int],
              x.fields("activity_other").convertTo[Nullable[String]],
              x.fields("name").convertTo[String],
              x.fields("created_at").convertTo[String],
              x.fields("uri").convertTo[String],
              x.fields("identification_code").convertTo[String],
              x.fields("legal_status").convertTo[String],
              x.fields("members_100").convertTo[Nullable[Int]],
              x.fields("head_office_lon").convertTo[Nullable[Double]],
              x.fields("structure_members").convertTo[String],
              x.fields("code_of_conduct").convertTo[String],
            )
          }
        }
        jsonFormat7(EuLobbyRepr.RootInterface)

      case "github-events.json" ⇒
        implicit val a = jsonFormat6(GithubEvents.Actor)
        implicit val b = jsonFormat2(GithubEvents.Author)
        implicit val c = jsonFormat1(GithubEvents.Self)
        implicit val d = jsonFormat18(GithubEvents.Owner)
        implicit val e = jsonFormat5(GithubEvents.Org)
        implicit val f = jsonFormat8(GithubEvents.Links)
        implicit val g = jsonFormat3(GithubEvents.Links1)
        implicit val h = jsonFormat6(GithubEvents.Labels)
        implicit val i = jsonFormat5(GithubEvents.License)
        implicit val j = jsonFormat3(GithubEvents.Repo)
        implicit val k = new JsonFormat[GithubEvents.Repo1] {
          def write(obj: GithubEvents.Repo1) = ???
          def read(json: JsValue) = {
            val x = json.asJsObject
            GithubEvents.Repo1(
              x.fields("id").convertTo[Long],
              x.fields("node_id").convertTo[String],
              x.fields("name").convertTo[String],
              x.fields("full_name").convertTo[String],
              x.fields("private").convertTo[Boolean],
              x.fields("owner").convertTo[GithubEvents.Owner],
              x.fields("html_url").convertTo[String],
              x.fields("description").convertTo[Nullable[String]],
              x.fields("fork").convertTo[Boolean],
              x.fields("url").convertTo[String],
              x.fields("forks_url").convertTo[String],
              x.fields("keys_url").convertTo[String],
              x.fields("collaborators_url").convertTo[String],
              x.fields("teams_url").convertTo[String],
              x.fields("hooks_url").convertTo[String],
              x.fields("issue_events_url").convertTo[String],
              x.fields("events_url").convertTo[String],
              x.fields("assignees_url").convertTo[String],
              x.fields("branches_url").convertTo[String],
              x.fields("tags_url").convertTo[String],
              x.fields("blobs_url").convertTo[String],
              x.fields("git_tags_url").convertTo[String],
              x.fields("git_refs_url").convertTo[String],
              x.fields("trees_url").convertTo[String],
              x.fields("statuses_url").convertTo[String],
              x.fields("languages_url").convertTo[String],
              x.fields("stargazers_url").convertTo[String],
              x.fields("contributors_url").convertTo[String],
              x.fields("subscribers_url").convertTo[String],
              x.fields("subscription_url").convertTo[String],
              x.fields("commits_url").convertTo[String],
              x.fields("git_commits_url").convertTo[String],
              x.fields("comments_url").convertTo[String],
              x.fields("issue_comment_url").convertTo[String],
              x.fields("contents_url").convertTo[String],
              x.fields("compare_url").convertTo[String],
              x.fields("merges_url").convertTo[String],
              x.fields("archive_url").convertTo[String],
              x.fields("downloads_url").convertTo[String],
              x.fields("issues_url").convertTo[String],
              x.fields("pulls_url").convertTo[String],
              x.fields("milestones_url").convertTo[String],
              x.fields("notifications_url").convertTo[String],
              x.fields("labels_url").convertTo[String],
              x.fields("releases_url").convertTo[String],
              x.fields("deployments_url").convertTo[String],
              x.fields("created_at").convertTo[String],
              x.fields("updated_at").convertTo[String],
              x.fields("pushed_at").convertTo[String],
              x.fields("git_url").convertTo[String],
              x.fields("ssh_url").convertTo[String],
              x.fields("clone_url").convertTo[String],
              x.fields("svn_url").convertTo[String],
              x.fields("homepage").convertTo[Nullable[String]],
              x.fields("size").convertTo[Int],
              x.fields("stargazers_count").convertTo[Int],
              x.fields("watchers_count").convertTo[Int],
              x.fields("language").convertTo[String],
              x.fields("has_issues").convertTo[Boolean],
              x.fields("has_projects").convertTo[Boolean],
              x.fields("has_downloads").convertTo[Boolean],
              x.fields("has_wiki").convertTo[Boolean],
              x.fields("has_pages").convertTo[Boolean],
              x.fields("forks_count").convertTo[Int],
              x.fields("mirror_url").convertTo[Nullable[String]],
              x.fields("archived").convertTo[Boolean],
              x.fields("open_issues_count").convertTo[Int],
              x.fields("license").convertTo[GithubEvents.License],
              x.fields("forks").convertTo[Int],
              x.fields("open_issues").convertTo[Int],
              x.fields("watchers").convertTo[Int],
              x.fields("default_branch").convertTo[String],
            )
          }
        }
        implicit val l = jsonFormat10(GithubEvents.Comment)
        implicit val m = jsonFormat19(GithubEvents.Comment1)
        implicit val n = jsonFormat5(GithubEvents.Commits)
        implicit val o = jsonFormat5(GithubEvents.CreateEvent)
        implicit val p = new JsonFormat[GithubEvents.Forkee] {
          def write(obj: GithubEvents.Forkee) = ???
          def read(json: JsValue) = {
            val x = json.asJsObject
            GithubEvents.Forkee(
              x.fields("id").convertTo[Long],
              x.fields("node_id").convertTo[String],
              x.fields("name").convertTo[String],
              x.fields("full_name").convertTo[String],
              x.fields("private").convertTo[Boolean],
              x.fields("owner").convertTo[GithubEvents.Owner],
              x.fields("html_url").convertTo[String],
              x.fields("description").convertTo[Nullable[String]],
              x.fields("fork").convertTo[Boolean],
              x.fields("url").convertTo[String],
              x.fields("forks_url").convertTo[String],
              x.fields("keys_url").convertTo[String],
              x.fields("collaborators_url").convertTo[String],
              x.fields("teams_url").convertTo[String],
              x.fields("hooks_url").convertTo[String],
              x.fields("issue_events_url").convertTo[String],
              x.fields("events_url").convertTo[String],
              x.fields("assignees_url").convertTo[String],
              x.fields("branches_url").convertTo[String],
              x.fields("tags_url").convertTo[String],
              x.fields("blobs_url").convertTo[String],
              x.fields("git_tags_url").convertTo[String],
              x.fields("git_refs_url").convertTo[String],
              x.fields("trees_url").convertTo[String],
              x.fields("statuses_url").convertTo[String],
              x.fields("languages_url").convertTo[String],
              x.fields("stargazers_url").convertTo[String],
              x.fields("contributors_url").convertTo[String],
              x.fields("subscribers_url").convertTo[String],
              x.fields("subscription_url").convertTo[String],
              x.fields("commits_url").convertTo[String],
              x.fields("git_commits_url").convertTo[String],
              x.fields("comments_url").convertTo[String],
              x.fields("issue_comment_url").convertTo[String],
              x.fields("contents_url").convertTo[String],
              x.fields("compare_url").convertTo[String],
              x.fields("merges_url").convertTo[String],
              x.fields("archive_url").convertTo[String],
              x.fields("downloads_url").convertTo[String],
              x.fields("issues_url").convertTo[String],
              x.fields("pulls_url").convertTo[String],
              x.fields("milestones_url").convertTo[String],
              x.fields("notifications_url").convertTo[String],
              x.fields("labels_url").convertTo[String],
              x.fields("releases_url").convertTo[String],
              x.fields("deployments_url").convertTo[String],
              x.fields("created_at").convertTo[String],
              x.fields("updated_at").convertTo[String],
              x.fields("pushed_at").convertTo[String],
              x.fields("git_url").convertTo[String],
              x.fields("ssh_url").convertTo[String],
              x.fields("clone_url").convertTo[String],
              x.fields("svn_url").convertTo[String],
              x.fields("homepage").convertTo[Nullable[String]],
              x.fields("size").convertTo[Int],
              x.fields("stargazers_count").convertTo[Int],
              x.fields("watchers_count").convertTo[Int],
              x.fields("language").convertTo[Nullable[String]],
              x.fields("has_issues").convertTo[Boolean],
              x.fields("has_projects").convertTo[Boolean],
              x.fields("has_downloads").convertTo[Boolean],
              x.fields("has_wiki").convertTo[Boolean],
              x.fields("has_pages").convertTo[Boolean],
              x.fields("forks_count").convertTo[Int],
              x.fields("mirror_url").convertTo[Nullable[String]],
              x.fields("archived").convertTo[Boolean],
              x.fields("open_issues_count").convertTo[Int],
              x.fields("license").convertTo[Nullable[String]],
              x.fields("forks").convertTo[Int],
              x.fields("open_issues").convertTo[Int],
              x.fields("watchers").convertTo[Int],
              x.fields("default_branch").convertTo[String],
              x.fields("public").convertTo[Boolean],
            )
          }
        }
        implicit val q = jsonFormat1(GithubEvents.ForkEvent)
        implicit val r = jsonFormat5(GithubEvents.Head)
        implicit val s = jsonFormat4(GithubEvents.PullRequest)
        implicit val t = new JsonFormat[GithubEvents.PullRequest1] {
          def write(obj: GithubEvents.PullRequest1) = ???
          def read(json: JsValue) = {
            val x = json.asJsObject
            GithubEvents.PullRequest1(
              x.fields("url").convertTo[String],
              x.fields("id").convertTo[Long],
              x.fields("node_id").convertTo[String],
              x.fields("html_url").convertTo[String],
              x.fields("diff_url").convertTo[String],
              x.fields("patch_url").convertTo[String],
              x.fields("issue_url").convertTo[String],
              x.fields("number").convertTo[Int],
              x.fields("state").convertTo[String],
              x.fields("locked").convertTo[Boolean],
              x.fields("title").convertTo[String],
              x.fields("user").convertTo[GithubEvents.Owner],
              x.fields("body").convertTo[String],
              x.fields("created_at").convertTo[String],
              x.fields("updated_at").convertTo[String],
              x.fields("closed_at").convertTo[Nullable[String]],
              x.fields("merged_at").convertTo[Nullable[String]],
              x.fields("merge_commit_sha").convertTo[Nullable[String]],
              x.fields("assignee").convertTo[Nullable[String]],
              x.fields("assignees").convertTo[Seq[String]],
              x.fields("requested_reviewers").convertTo[Seq[String]],
              x.fields("requested_teams").convertTo[Seq[String]],
              x.fields("labels").convertTo[Seq[GithubEvents.Labels]],
              x.fields("milestone").convertTo[Nullable[String]],
              x.fields("commits_url").convertTo[String],
              x.fields("review_comments_url").convertTo[String],
              x.fields("review_comment_url").convertTo[String],
              x.fields("comments_url").convertTo[String],
              x.fields("statuses_url").convertTo[String],
              x.fields("head").convertTo[GithubEvents.Head],
              x.fields("base").convertTo[GithubEvents.Head],
              x.fields("_links").convertTo[GithubEvents.Links],
              x.fields("author_association").convertTo[String],
              x.fields("merged").convertTo[Boolean],
              x.fields("mergeable").convertTo[Nullable[String]],
              x.fields("rebaseable").convertTo[Nullable[String]],
              x.fields("mergeable_state").convertTo[String],
              x.fields("merged_by").convertTo[Nullable[String]],
              x.fields("comments").convertTo[Int],
              x.fields("review_comments").convertTo[Int],
              x.fields("maintainer_can_modify").convertTo[Boolean],
              x.fields("commits").convertTo[Int],
              x.fields("additions").convertTo[Int],
              x.fields("deletions").convertTo[Int],
              x.fields("changed_files").convertTo[Int],
            )
          }
        }
        implicit val u = new JsonFormat[GithubEvents.PullRequest2] {
          def write(obj: GithubEvents.PullRequest2) = ???
          def read(json: JsValue) = {
            val x = json.asJsObject
            GithubEvents.PullRequest2(
              x.fields("url").convertTo[String],
              x.fields("id").convertTo[Long],
              x.fields("node_id").convertTo[String],
              x.fields("html_url").convertTo[String],
              x.fields("diff_url").convertTo[String],
              x.fields("patch_url").convertTo[String],
              x.fields("issue_url").convertTo[String],
              x.fields("number").convertTo[Int],
              x.fields("state").convertTo[String],
              x.fields("locked").convertTo[Boolean],
              x.fields("title").convertTo[String],
              x.fields("user").convertTo[GithubEvents.Owner],
              x.fields("body").convertTo[String],
              x.fields("created_at").convertTo[String],
              x.fields("updated_at").convertTo[String],
              x.fields("closed_at").convertTo[Nullable[String]],
              x.fields("merged_at").convertTo[Nullable[String]],
              x.fields("merge_commit_sha").convertTo[String],
              x.fields("assignee").convertTo[GithubEvents.Owner],
              x.fields("assignees").convertTo[Seq[GithubEvents.Owner]],
              x.fields("requested_reviewers").convertTo[Seq[String]],
              x.fields("requested_teams").convertTo[Seq[String]],
              x.fields("labels").convertTo[Seq[String]],
              x.fields("milestone").convertTo[Nullable[String]],
              x.fields("commits_url").convertTo[String],
              x.fields("review_comments_url").convertTo[String],
              x.fields("review_comment_url").convertTo[String],
              x.fields("comments_url").convertTo[String],
              x.fields("statuses_url").convertTo[String],
              x.fields("head").convertTo[GithubEvents.Head],
              x.fields("base").convertTo[GithubEvents.Head],
              x.fields("_links").convertTo[GithubEvents.Links],
              x.fields("author_association").convertTo[String],
            )
          }
        }
        implicit val v = jsonFormat3(GithubEvents.PullRequestEvent)
        implicit val w = jsonFormat3(GithubEvents.PullRequestReviewCommentEvent)
        implicit val x = new JsonFormat[GithubEvents.Issue] {
          def write(obj: GithubEvents.Issue) = ???
          def read(json: JsValue) = {
            val x = json.asJsObject
            GithubEvents.Issue(
              x.fields("url").convertTo[String],
              x.fields("repository_url").convertTo[String],
              x.fields("labels_url").convertTo[String],
              x.fields("comments_url").convertTo[String],
              x.fields("events_url").convertTo[String],
              x.fields("html_url").convertTo[String],
              x.fields("id").convertTo[Long],
              x.fields("node_id").convertTo[String],
              x.fields("number").convertTo[Int],
              x.fields("title").convertTo[String],
              x.fields("user").convertTo[GithubEvents.Owner],
              x.fields("labels").convertTo[Seq[GithubEvents.Labels]],
              x.fields("state").convertTo[String],
              x.fields("locked").convertTo[Boolean],
              x.fields("assignee").convertTo[Nullable[String]],
              x.fields("assignees").convertTo[Seq[String]],
              x.fields("milestone").convertTo[Nullable[String]],
              x.fields("comments").convertTo[Int],
              x.fields("created_at").convertTo[String],
              x.fields("updated_at").convertTo[String],
              x.fields("closed_at").convertTo[Nullable[String]],
              x.fields("author_association").convertTo[String],
              x.fields("pull_request").convertTo[GithubEvents.PullRequest],
              x.fields("body").convertTo[String],
            )
          }
        }
        implicit val y =  new JsonFormat[GithubEvents.Issue1] {
          def write(obj: GithubEvents.Issue1) = ???
          def read(json: JsValue) = {
            val x = json.asJsObject
            GithubEvents.Issue1(
              x.fields("url").convertTo[String],
              x.fields("repository_url").convertTo[String],
              x.fields("labels_url").convertTo[String],
              x.fields("comments_url").convertTo[String],
              x.fields("events_url").convertTo[String],
              x.fields("html_url").convertTo[String],
              x.fields("id").convertTo[Long],
              x.fields("node_id").convertTo[String],
              x.fields("number").convertTo[Int],
              x.fields("title").convertTo[String],
              x.fields("user").convertTo[GithubEvents.Owner],
              x.fields("labels").convertTo[Seq[String]],
              x.fields("state").convertTo[String],
              x.fields("locked").convertTo[Boolean],
              x.fields("assignee").convertTo[Nullable[String]],
              x.fields("assignees").convertTo[Seq[String]],
              x.fields("milestone").convertTo[Nullable[String]],
              x.fields("comments").convertTo[Int],
              x.fields("created_at").convertTo[String],
              x.fields("updated_at").convertTo[String],
              x.fields("closed_at").convertTo[Nullable[String]],
              x.fields("author_association").convertTo[String],
              x.fields("body").convertTo[String],
            )
          }
        }
        implicit val z = jsonFormat3(GithubEvents.IssueCommentEvent)
        implicit val A = jsonFormat2(GithubEvents.IssuesEvent)
        implicit val B = jsonFormat7(GithubEvents.PushEvent)
        implicit val C = jsonFormat1(GithubEvents.WatchEvent)
        implicit val D = jsonFormat15(GithubEvents.RootInterface)
        implicitly[RootJsonFormat[List[GithubEvents.RootInterface]]]

      case "github-gists.json" ⇒
        implicit val a = jsonFormat18(GithubGists.Owner)
        implicit val b = jsonFormat5(GithubGists.FileData)
        implicit val c = jsonFormat18(GithubGists.RootInterface)
        implicitly[RootJsonFormat[List[GithubGists.RootInterface]]]

      case "json-generator.json" ⇒
        implicit val a = jsonFormat2(JsonGenerator.Friends)
        implicit val b = jsonFormat2(JsonGenerator.Name)
        implicit val c = jsonFormat22(JsonGenerator.RootInterface)
        implicitly[RootJsonFormat[List[JsonGenerator.RootInterface]]]

      case "meteorites.json" ⇒
        implicit val a = jsonFormat2(Meteorites.Geolocation)
        implicit val b = jsonFormat12(Meteorites.RootInterface)
        implicitly[RootJsonFormat[List[Meteorites.RootInterface]]]

      case "movies.json" ⇒
        implicit val a = jsonFormat4(Movies.RootInterface)
        implicitly[RootJsonFormat[List[Movies.RootInterface]]]

      case "reddit-scala.json" ⇒
        implicit val a = jsonFormat13(Reddit.Oembed)
        implicit val b = jsonFormat2(Reddit.SecureMedia)
        implicit val c = jsonFormat5(Reddit.MediaEmbed)
        implicit val d = jsonFormat3(Reddit.Gildings)
        implicit val e = new JsonFormat[Reddit.Data] {
          def write(obj: Reddit.Data) = ???
          def read(json: JsValue) = {
            val x = json.asJsObject
            Reddit.Data(
              x.fields("approved_at_utc").convertTo[Nullable[String]],
              x.fields("subreddit").convertTo[String],
              x.fields("selftext").convertTo[String],
              x.fields("author_fullname").convertTo[String],
              x.fields("saved").convertTo[Boolean],
              x.fields("mod_reason_title").convertTo[Nullable[String]],
              x.fields("gilded").convertTo[Int],
              x.fields("clicked").convertTo[Boolean],
              x.fields("title").convertTo[String],
              x.fields("link_flair_richtext").convertTo[Seq[String]],
              x.fields("subreddit_name_prefixed").convertTo[String],
              x.fields("hidden").convertTo[Boolean],
              x.fields("pwls").convertTo[Int],
              x.fields("link_flair_css_class").convertTo[Nullable[String]],
              x.fields("downs").convertTo[Int],
              x.fields("parent_whitelist_status").convertTo[String],
              x.fields("hide_score").convertTo[Boolean],
              x.fields.get("name").map(_.convertTo[String]),
              x.fields.get("quarantine").map(_.convertTo[Boolean]),
              x.fields("link_flair_text_color").convertTo[String],
              x.fields("author_flair_background_color").convertTo[Nullable[String]],
              x.fields("subreddit_type").convertTo[String],
              x.fields("ups").convertTo[Int],
              x.fields("domain").convertTo[String],
              x.fields("media_embed").convertTo[List[Reddit.MediaEmbed]],
              x.fields("author_flair_template_id").convertTo[Nullable[String]],
              x.fields("is_original_content").convertTo[Boolean],
              x.fields("user_reports").convertTo[Seq[String]],
              x.fields("secure_media").convertTo[Nullable[Option[Reddit.SecureMedia]]],
              x.fields("is_reddit_media_domain").convertTo[Boolean],
              x.fields("is_meta").convertTo[Boolean],
              x.fields("category").convertTo[Nullable[String]],
              x.fields("secure_media_embed").convertTo[Nullable[Option[Reddit.MediaEmbed]]],
              x.fields("link_flair_text").convertTo[Nullable[String]],
              x.fields("can_mod_post").convertTo[Boolean],
              x.fields("score").convertTo[Int],
              x.fields("approved_by").convertTo[Nullable[String]],
              x.fields("thumbnail").convertTo[String],
              x.fields("edited").convertTo[Boolean],
              x.fields("author_flair_css_class").convertTo[Nullable[String]],
              x.fields("author_flair_richtext").convertTo[Seq[String]],
              x.fields("gildings").convertTo[Reddit.Gildings],
              x.fields("content_categories").convertTo[Nullable[String]],
              x.fields("is_self").convertTo[Boolean],
              x.fields("mod_note").convertTo[Nullable[String]],
              x.fields("created").convertTo[Double],
              x.fields("link_flair_type").convertTo[String],
              x.fields("wls").convertTo[Int],
              x.fields("banned_by").convertTo[Nullable[String]],
              x.fields("author_flair_type").convertTo[String],
              x.fields("contest_mode").convertTo[Boolean],
              x.fields("selftext_html").convertTo[Nullable[String]],
              x.fields("likes").convertTo[Nullable[String]],
              x.fields("suggested_sort").convertTo[Nullable[String]],
              x.fields("banned_at_utc").convertTo[Nullable[String]],
              x.fields("view_count").convertTo[Nullable[String]],
              x.fields("archived").convertTo[Boolean],
              x.fields("no_follow").convertTo[Boolean],
              x.fields("is_crosspostable").convertTo[Boolean],
              x.fields("pinned").convertTo[Boolean],
              x.fields("over_18").convertTo[Boolean],
              x.fields("media_only").convertTo[Boolean],
              x.fields("can_gild").convertTo[Boolean],
              x.fields("spoiler").convertTo[Boolean],
              x.fields("locked").convertTo[Boolean],
              x.fields("author_flair_text").convertTo[Nullable[String]],
              x.fields("visited").convertTo[Boolean],
              x.fields("num_reports").convertTo[Nullable[String]],
              x.fields("distinguished").convertTo[Nullable[String]],
              x.fields("subreddit_id").convertTo[String],
              x.fields("mod_reason_by").convertTo[Nullable[String]],
              x.fields("removal_reason").convertTo[Nullable[String]],
              x.fields("link_flair_background_color").convertTo[String],
              x.fields("id").convertTo[String],
              x.fields("is_robot_indexable").convertTo[Boolean],
              x.fields("report_reasons").convertTo[Nullable[String]],
              x.fields("author").convertTo[String],
              x.fields("num_crossposts").convertTo[Int],
              x.fields("num_comments").convertTo[Int],
              x.fields("send_replies").convertTo[Boolean],
              x.fields("mod_reports").convertTo[Seq[String]],
              x.fields("author_patreon_flair").convertTo[Boolean],
              x.fields("author_flair_text_color").convertTo[Nullable[String]],
              x.fields("permalink").convertTo[String],
              x.fields("whitelist_status").convertTo[String],
              x.fields("stickied").convertTo[Boolean],
              x.fields("url").convertTo[String],
              x.fields("subreddit_subscribers").convertTo[Int],
              x.fields("created_utc").convertTo[Double],
              x.fields.get("media").map(_.convertTo[Reddit.SecureMedia]),
              x.fields("is_video").convertTo[Boolean],
            )
          }
        }
        implicit val f = jsonFormat2(Reddit.Child)
        implicit val g = jsonFormat5(Reddit.Data0)
        jsonFormat2(Reddit.RootInterface)

      case "rick-morty.json" ⇒
        implicit val a = jsonFormat1(RickMorty.Rating)
        implicit val b = jsonFormat2(RickMorty.Schedule)
        implicit val c = jsonFormat3(RickMorty.Country)
        implicit val d = jsonFormat3(RickMorty.Network)
        implicit val e = jsonFormat2(RickMorty.Image)
        implicit val f = jsonFormat3(RickMorty.Externals)
        implicit val g = jsonFormat1(RickMorty.Self)
        implicit val h = jsonFormat2(RickMorty.Links)
        implicit val i = jsonFormat1(RickMorty.Links1)
        implicit val j = jsonFormat12(RickMorty.Episodes)
        implicit val k = jsonFormat1(RickMorty.Embedded)
        jsonFormat21(RickMorty.RootInterface)

      case "temp-anomaly.json" ⇒
        implicit val a = jsonFormat4(TempAnomaly.Description)
        jsonFormat2(TempAnomaly.RootInterface)

      case "thai-cinemas.json" ⇒
        implicit val a = jsonFormat4(ThaiCinemas.Group)
        implicit val b = jsonFormat17(ThaiCinemas.Results)
        jsonFormat4(ThaiCinemas.RootInterface)

      case "turkish.json" ⇒
        implicit val a = jsonFormat6(Turkish.Event)
        implicit val b = jsonFormat2(Turkish.Result)
        jsonFormat1(Turkish.RootInterface)

      case "twitter_api_compact_response.json" | "twitter_api_response.json" ⇒
        implicit val a = jsonFormat4(TwitterApiResponse.Urls)
        implicit val b = jsonFormat1(TwitterApiResponse.Url)
        implicit val c = jsonFormat5(TwitterApiResponse.UserMentions)
        implicit val d = jsonFormat2(TwitterApiResponse.Entities)
        implicit val e = jsonFormat2(TwitterApiResponse.Entities1)
        implicit val f = new JsonFormat[TwitterApiResponse.User] {
          def write(obj: TwitterApiResponse.User) = ???
          def read(json: JsValue) = {
            val x = json.asJsObject
            TwitterApiResponse.User(
              x.fields("id").convertTo[Long],
              x.fields("id_str").convertTo[String],
              x.fields("name").convertTo[String],
              x.fields("screen_name").convertTo[String],
              x.fields("location").convertTo[String],
              x.fields("description").convertTo[String],
              x.fields("url").convertTo[String],
              x.fields("entities").convertTo[TwitterApiResponse.Entities1],
              x.fields("protected").convertTo[Boolean],
              x.fields("followers_count").convertTo[Int],
              x.fields("friends_count").convertTo[Int],
              x.fields("listed_count").convertTo[Int],
              x.fields("created_at").convertTo[String],
              x.fields("favourites_count").convertTo[Int],
              x.fields("utc_offset").convertTo[Int],
              x.fields("time_zone").convertTo[String],
              x.fields("geo_enabled").convertTo[Boolean],
              x.fields("verified").convertTo[Boolean],
              x.fields("statuses_count").convertTo[Int],
              x.fields("lang").convertTo[String],
              x.fields("contributors_enabled").convertTo[Boolean],
              x.fields("is_translator").convertTo[Boolean],
              x.fields("is_translation_enabled").convertTo[Boolean],
              x.fields("profile_background_color").convertTo[String],
              x.fields("profile_background_image_url").convertTo[String],
              x.fields("profile_background_image_url_https").convertTo[String],
              x.fields("profile_background_tile").convertTo[Boolean],
              x.fields("profile_image_url").convertTo[String],
              x.fields("profile_image_url_https").convertTo[String],
              x.fields("profile_banner_url").convertTo[String],
              x.fields("profile_link_color").convertTo[String],
              x.fields("profile_sidebar_border_color").convertTo[String],
              x.fields("profile_sidebar_fill_color").convertTo[String],
              x.fields("profile_text_color").convertTo[String],
              x.fields("profile_use_background_image").convertTo[Boolean],
              x.fields("has_extended_profile").convertTo[Boolean],
              x.fields("default_profile").convertTo[Boolean],
              x.fields("default_profile_image").convertTo[Boolean],
              x.fields("following").convertTo[Boolean],
              x.fields("follow_request_sent").convertTo[Boolean],
              x.fields("notifications").convertTo[Boolean],
              x.fields("translator_type").convertTo[String],
            )
          }
        }
        implicit val g = jsonFormat15(TwitterApiResponse.RetweetedStatus)
        implicit val h = jsonFormat16(TwitterApiResponse.RootInterface)
        implicitly[RootJsonFormat[List[TwitterApiResponse.RootInterface]]]
    }
    c.asInstanceOf[RootJsonFormat[Product]]
  }

  def setup(): Unit = root = JsonParser(new String(fileBytes, UTF_8)).convertTo[Product]

  @Benchmark
  def encodeModel: Array[Byte] = root.toJson.compactPrint.getBytes(UTF_8)

  @Benchmark
  def decodeModel: Product = JsonParser(new String(fileBytes, UTF_8)).convertTo[Product]
}
