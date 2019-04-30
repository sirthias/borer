/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.benchmarks

import io.bullet.borer.Nullable

// model for the test JSON files, generated with https://transform.now.sh/json-to-scala-case-class/

object Australia {

  case class Crs(
      `type`: String,
      properties: Properties1
  )

  case class Features(
      `type`: String,
      id: String,
      geometry: Geometry,
      geometry_name: String,
      properties: Properties
  )

  case class Geometry(
      `type`: String,
      coordinates: Seq[Double]
  )

  case class Properties(
      name: String,
      streetaddress: String,
      twitteraccount: String,
      facebookaccount: String,
      siteurl: String,
      frequencyfinderurl: String
  )

  case class Properties1(
      name: String
  )

  case class RootInterface(
      `type`: String,
      totalFeatures: Int,
      features: Seq[Features],
      crs: Crs
  )
}

object Bitcoin {
  case class Inputs(
      sequence: Long,
      witness: String,
      prev_out: PrevOut,
      script: String
  )

  case class Out(
      spent: Boolean,
      tx_index: Int,
      `type`: Int,
      addr: Option[String] = None,
      value: Int,
      n: Int,
      script: String
  )

  case class PrevOut(
      spent: Boolean,
      spending_outpoints: Seq[SpendingOutpoints],
      tx_index: Int,
      `type`: Int,
      addr: String,
      value: Int,
      n: Int,
      script: String
  )

  case class RootInterface(
      txs: Seq[Txs]
  )

  case class SpendingOutpoints(
      tx_index: Int,
      n: Int
  )

  case class Txs(
      ver: Int,
      inputs: Seq[Inputs],
      weight: Int,
      relayed_by: String,
      out: Seq[Out],
      lock_time: Int,
      size: Int,
      double_spend: Boolean,
      time: Int,
      tx_index: Int,
      vin_sz: Int,
      hash: String,
      vout_sz: Int
  )

}

object DojBlog {
  case class Component(
      uuid: String,
      name: String
  )

  case class Metadata(
      responseInfo: ResponseInfo,
      resultset: Resultset,
      executionTime: Double
  )

  case class ResponseInfo(
      status: Int,
      developerMessage: String
  )

  case class Results(
      attachments: Seq[String],
      body: String,
      changed: String,
      component: Seq[Component],
      created: String,
      date: String,
      image: Seq[String],
      teaser: Seq[String],
      title: String,
      topic: Seq[String],
      url: String,
      uuid: String,
      vuuid: String
  )

  case class Resultset(
      count: Int,
      pagesize: Int,
      page: Int
  )

  case class RootInterface(
      metadata: Metadata,
      results: Seq[Results]
  )

}

object EuLobbyCountry {
  case class Facets(
      )

  case class Results(
      name: String,
      acronym: Nullable[String],
      created_at: String,
      uri: String,
      updated_at: String,
      id: String
  )

  case class RootInterface(
      count: Int,
      facets: Facets,
      results: Seq[Results],
      next: String,
      limit: Int,
      offset: Int,
      previous: Boolean
  )
}

object EuLobbyFinancial {
  case class Facets()

  case class CustomIncomes(
      status: String,
      name: String,
      created_at: String,
      updated_at: String,
      uri: String,
      amount: Int,
      `type`: String,
      id: String
  )

  case class Results(
      other_sources_donation: Nullable[Int],
      status: String,
      turnover_min: Nullable[Int],
      eur_sources_procurement: Int,
      end_date: String,
      eur_sources_procurement_src: Nullable[String],
      new_organisation: Nullable[String],
      turnover_max: Nullable[Int],
      updated_at: String,
      cost_min: Nullable[Int],
      direct_rep_costs_max: Nullable[Int],
      representative: String,
      cost_absolute: Nullable[Int],
      eur_sources_grants_src: Nullable[String],
      id: String,
      customIncomes: Seq[CustomIncomes],
      total_budget: Nullable[Int],
      turnover_absolute: Nullable[Int],
      eur_sources_grants: Int,
      other_sources_contributions: Nullable[Int],
      cost_max: Nullable[Int],
      created_at: String,
      uri: String,
      public_financing_infranational: Nullable[Int],
      direct_rep_costs_min: Nullable[Int],
      public_financing_total: Nullable[Int],
      other_sources_total: Nullable[Int],
      other_financial_information: Nullable[String],
      public_financing_national: Nullable[Int],
      no_clients: Nullable[String],
      `type`: String,
      start_date: String
  )

  case class RootInterface(
      count: Int,
      facets: Facets,
      results: Seq[Results],
      next: String,
      limit: Int,
      offset: Int,
      previous: Boolean
  )
}

object EuLobbyRepr {
  case class Facets()
  case class Results(
      activity_consult_committees: Nullable[String],
      activity_high_level_groups: Nullable[String],
      head_office_lat: Nullable[Double],
      be_office_lat: Nullable[Double] = 0.0,
      updated_at: String,
      entity: String,
      number_of_natural_persons: Nullable[Int],
      legal: String,
      native_name: Nullable[String],
      head_office_country: String,
      id: String,
      activity_industry_forums: String,
      contact_country: Int,
      head_office_postbox: Nullable[String],
      networking: Nullable[String],
      members_75: Nullable[Int],
      main_category: Int,
      members_50: Nullable[Int],
      activity_expert_groups: String,
      sub_category_title: String,
      other_code_of_conduct: Nullable[String],
      head_office_town: String,
      info_members: String,
      head: String,
      status: String,
      main_category_title: String,
      head_office_street: String,
      be_office_post_code: Nullable[String] = "",
      activity_inter_groups: String,
      acronym: Nullable[String],
      activity_eu_legislative: String,
      registration_date: String,
      activity_relevant_comm: String,
      head_office_post_code: Nullable[String],
      goals: String,
      members: Int,
      last_update_date: String,
      members_fte: Double,
      head_office_phone: String,
      be_office_town: String = "",
      members_25: Nullable[Int],
      web_site_url: Nullable[String],
      sub_category: Int,
      activity_other: Nullable[String],
      be_office_postbox: Nullable[String] = "",
      name: String,
      be_office_street: String = "",
      created_at: String,
      be_office_country: String = "",
      uri: String,
      identification_code: String,
      legal_status: String,
      members_100: Nullable[Int],
      be_office_phone: String = "",
      be_office_lon: Nullable[Double] = 0.0,
      head_office_lon: Nullable[Double],
      structure_members: String,
      code_of_conduct: String,
  )

  case class RootInterface(
      count: Int,
      facets: Facets,
      results: Seq[Results],
      next: String,
      limit: Int,
      offset: Int,
      previous: Boolean
  )
}

object GithubEvents {
  case class Actor(
      id: Long,
      login: String,
      display_login: String,
      gravatar_id: String,
      url: String,
      avatar_url: String
  )

  case class Author(
      email: String,
      name: String
  )

  case class Comment(
      url: String,
      html_url: String,
      issue_url: String,
      id: Long,
      node_id: String,
      user: Owner,
      created_at: String,
      updated_at: String,
      author_association: String,
      body: String
  )

  case class Comment1(
      url: String,
      pull_request_review_id: Long,
      id: Long,
      node_id: String,
      diff_hunk: String,
      path: String,
      position: Int,
      original_position: Int,
      commit_id: String,
      original_commit_id: String,
      user: Owner,
      body: String,
      created_at: String,
      updated_at: String,
      html_url: String,
      pull_request_url: String,
      author_association: String,
      _links: Links1,
      in_reply_to_id: Long
  )

  case class Commits(
      sha: String,
      author: Author,
      message: String,
      distinct: Boolean,
      url: String
  )

  case class CreateEvent(
      ref: Nullable[String],
      ref_type: String,
      master_branch: String,
      description: Nullable[String],
      pusher_type: String
  )

  case class ForkEvent(
      forkee: Forkee
  )

  case class Forkee(
      id: Long,
      node_id: String,
      name: String,
      full_name: String,
      `private`: Boolean,
      owner: Owner,
      html_url: String,
      description: Nullable[String],
      fork: Boolean,
      url: String,
      forks_url: String,
      keys_url: String,
      collaborators_url: String,
      teams_url: String,
      hooks_url: String,
      issue_events_url: String,
      events_url: String,
      assignees_url: String,
      branches_url: String,
      tags_url: String,
      blobs_url: String,
      git_tags_url: String,
      git_refs_url: String,
      trees_url: String,
      statuses_url: String,
      languages_url: String,
      stargazers_url: String,
      contributors_url: String,
      subscribers_url: String,
      subscription_url: String,
      commits_url: String,
      git_commits_url: String,
      comments_url: String,
      issue_comment_url: String,
      contents_url: String,
      compare_url: String,
      merges_url: String,
      archive_url: String,
      downloads_url: String,
      issues_url: String,
      pulls_url: String,
      milestones_url: String,
      notifications_url: String,
      labels_url: String,
      releases_url: String,
      deployments_url: String,
      created_at: String,
      updated_at: String,
      pushed_at: String,
      git_url: String,
      ssh_url: String,
      clone_url: String,
      svn_url: String,
      homepage: Nullable[String],
      size: Int,
      stargazers_count: Int,
      watchers_count: Int,
      language: Nullable[String],
      has_issues: Boolean,
      has_projects: Boolean,
      has_downloads: Boolean,
      has_wiki: Boolean,
      has_pages: Boolean,
      forks_count: Int,
      mirror_url: Nullable[String],
      archived: Boolean,
      open_issues_count: Int,
      license: Nullable[String],
      forks: Int,
      open_issues: Int,
      watchers: Int,
      default_branch: String,
      public: Boolean
  )

  case class Head(
      label: String,
      ref: String,
      sha: String,
      user: Owner,
      repo: Repo1
  )

  case class Issue(
      url: String,
      repository_url: String,
      labels_url: String,
      comments_url: String,
      events_url: String,
      html_url: String,
      id: Long,
      node_id: String,
      number: Int,
      title: String,
      user: Owner,
      labels: Seq[Labels],
      state: String,
      locked: Boolean,
      assignee: Nullable[String],
      assignees: Seq[String],
      milestone: Nullable[String],
      comments: Int,
      created_at: String,
      updated_at: String,
      closed_at: Nullable[String],
      author_association: String,
      pull_request: PullRequest,
      body: String
  )

  case class Issue1(
      url: String,
      repository_url: String,
      labels_url: String,
      comments_url: String,
      events_url: String,
      html_url: String,
      id: Long,
      node_id: String,
      number: Int,
      title: String,
      user: Owner,
      labels: Seq[String],
      state: String,
      locked: Boolean,
      assignee: Nullable[String],
      assignees: Seq[String],
      milestone: Nullable[String],
      comments: Int,
      created_at: String,
      updated_at: String,
      closed_at: Nullable[String],
      author_association: String,
      body: String
  )

  case class IssueCommentEvent(
      action: String,
      issue: Issue,
      comment: Comment
  )

  case class IssuesEvent(
      action: String,
      issue: Issue1
  )

  case class Labels(
      id: Long,
      node_id: String,
      url: String,
      name: String,
      color: String,
      default: Boolean
  )

  case class License(
      key: String,
      name: String,
      spdx_id: String,
      url: String,
      node_id: String
  )

  case class Links(
      self: Self,
      html: Self,
      issue: Self,
      comments: Self,
      review_comments: Self,
      review_comment: Self,
      commits: Self,
      statuses: Self
  )

  case class Links1(
      self: Self,
      html: Self,
      pull_request: Self
  )

  case class Org(
      id: Long,
      login: String,
      gravatar_id: String,
      url: String,
      avatar_url: String
  )

  case class Owner(
      login: String,
      id: Long,
      node_id: String,
      avatar_url: String,
      gravatar_id: String,
      url: String,
      html_url: String,
      followers_url: String,
      following_url: String,
      gists_url: String,
      starred_url: String,
      subscriptions_url: String,
      organizations_url: String,
      repos_url: String,
      events_url: String,
      received_events_url: String,
      `type`: String,
      site_admin: Boolean
  )

  case class PullRequest(
      url: String,
      html_url: String,
      diff_url: String,
      patch_url: String
  )

  case class PullRequest1(
      url: String,
      id: Long,
      node_id: String,
      html_url: String,
      diff_url: String,
      patch_url: String,
      issue_url: String,
      number: Int,
      state: String,
      locked: Boolean,
      title: String,
      user: Owner,
      body: String,
      created_at: String,
      updated_at: String,
      closed_at: Nullable[String],
      merged_at: Nullable[String],
      merge_commit_sha: Nullable[String],
      assignee: Nullable[String],
      assignees: Seq[String],
      requested_reviewers: Seq[String],
      requested_teams: Seq[String],
      labels: Seq[Labels],
      milestone: Nullable[String],
      commits_url: String,
      review_comments_url: String,
      review_comment_url: String,
      comments_url: String,
      statuses_url: String,
      head: Head,
      base: Head,
      _links: Links,
      author_association: String,
      merged: Boolean,
      mergeable: Nullable[String],
      rebaseable: Nullable[String],
      mergeable_state: String,
      merged_by: Nullable[String],
      comments: Int,
      review_comments: Int,
      maintainer_can_modify: Boolean,
      commits: Int,
      additions: Int,
      deletions: Int,
      changed_files: Int
  )

  case class PullRequest2(
      url: String,
      id: Long,
      node_id: String,
      html_url: String,
      diff_url: String,
      patch_url: String,
      issue_url: String,
      number: Int,
      state: String,
      locked: Boolean,
      title: String,
      user: Owner,
      body: String,
      created_at: String,
      updated_at: String,
      closed_at: Nullable[String],
      merged_at: Nullable[String],
      merge_commit_sha: String,
      assignee: Owner,
      assignees: Seq[Owner],
      requested_reviewers: Seq[String],
      requested_teams: Seq[String],
      labels: Seq[String],
      milestone: Nullable[String],
      commits_url: String,
      review_comments_url: String,
      review_comment_url: String,
      comments_url: String,
      statuses_url: String,
      head: Head,
      base: Head,
      _links: Links,
      author_association: String
  )

  case class PullRequestEvent(
      action: String,
      number: Int,
      pull_request: PullRequest1
  )

  case class PullRequestReviewCommentEvent(
      action: String,
      comment: Comment1,
      pull_request: PullRequest2
  )

  case class PushEvent(
      push_id: Long,
      size: Int,
      distinct_size: Int,
      ref: String,
      head: String,
      before: String,
      commits: Seq[Commits]
  )

  case class Repo(
      id: Long,
      name: String,
      url: String
  )

  case class Repo1(
      id: Long,
      node_id: String,
      name: String,
      full_name: String,
      `private`: Boolean,
      owner: Owner,
      html_url: String,
      description: Nullable[String],
      fork: Boolean,
      url: String,
      forks_url: String,
      keys_url: String,
      collaborators_url: String,
      teams_url: String,
      hooks_url: String,
      issue_events_url: String,
      events_url: String,
      assignees_url: String,
      branches_url: String,
      tags_url: String,
      blobs_url: String,
      git_tags_url: String,
      git_refs_url: String,
      trees_url: String,
      statuses_url: String,
      languages_url: String,
      stargazers_url: String,
      contributors_url: String,
      subscribers_url: String,
      subscription_url: String,
      commits_url: String,
      git_commits_url: String,
      comments_url: String,
      issue_comment_url: String,
      contents_url: String,
      compare_url: String,
      merges_url: String,
      archive_url: String,
      downloads_url: String,
      issues_url: String,
      pulls_url: String,
      milestones_url: String,
      notifications_url: String,
      labels_url: String,
      releases_url: String,
      deployments_url: String,
      created_at: String,
      updated_at: String,
      pushed_at: String,
      git_url: String,
      ssh_url: String,
      clone_url: String,
      svn_url: String,
      homepage: Nullable[String],
      size: Int,
      stargazers_count: Int,
      watchers_count: Int,
      language: String,
      has_issues: Boolean,
      has_projects: Boolean,
      has_downloads: Boolean,
      has_wiki: Boolean,
      has_pages: Boolean,
      forks_count: Int,
      mirror_url: Nullable[String],
      archived: Boolean,
      open_issues_count: Int,
      license: License,
      forks: Int,
      open_issues: Int,
      watchers: Int,
      default_branch: String
  )

  case class RootInterface(
      id: String,
      `type`: String,
      actor: Actor,
      repo: Repo,
      public: Boolean,
      created_at: String,
      org: Option[Org] = None,
      PushEvent: Option[PushEvent] = None,
      ForkEvent: Option[ForkEvent] = None,
      CreateEvent: Option[CreateEvent] = None,
      IssueCommentEvent: Option[IssueCommentEvent] = None,
      PullRequestEvent: Option[PullRequestEvent] = None,
      WatchEvent: Option[WatchEvent] = None,
      IssuesEvent: Option[IssuesEvent] = None,
      PullRequestReviewCommentEvent: Option[PullRequestReviewCommentEvent] = None,
  )

  case class Self(
      href: String
  )

  case class WatchEvent(
      action: String
  )
}

object GithubGists {

  case class FileData(
      filename: String,
      `type`: String,
      language: Nullable[String],
      raw_url: String,
      size: Int
  )

  case class Owner(
      login: String,
      id: Int,
      node_id: String,
      avatar_url: String,
      gravatar_id: String,
      url: String,
      html_url: String,
      followers_url: String,
      following_url: String,
      gists_url: String,
      starred_url: String,
      subscriptions_url: String,
      organizations_url: String,
      repos_url: String,
      events_url: String,
      received_events_url: String,
      `type`: String,
      site_admin: Boolean
  )

  case class RootInterface(
      url: String,
      forks_url: String,
      commits_url: String,
      id: String,
      node_id: String,
      git_pull_url: String,
      git_push_url: String,
      html_url: String,
      files: Map[String, FileData],
      public: Boolean,
      created_at: String,
      updated_at: String,
      description: Nullable[String],
      comments: Int,
      user: Nullable[String],
      comments_url: String,
      owner: Owner,
      truncated: Boolean
  )
}

object JsonGenerator {
  case class Friends(
      id: Int,
      name: String
  )

  case class Name(
      first: String,
      last: String
  )

  case class RootInterface(
      _id: String,
      index: Int,
      guid: String,
      isActive: Boolean,
      balance: String,
      picture: String,
      age: Int,
      eyeColor: String,
      name: Name,
      company: String,
      email: String,
      phone: String,
      address: String,
      about: String,
      registered: String,
      latitude: String,
      longitude: String,
      tags: Seq[String],
      range: Seq[Int],
      friends: Seq[Friends],
      greeting: String,
      favoriteFruit: String
  )
}

object Meteorites {
  case class Geolocation(
      `type`: String,
      coordinates: List[Double]
  )

  case class RootInterface(
      fall: String,
      geolocation: Option[Geolocation] = None,
      id: String,
      mass: Option[String] = None,
      name: String,
      nametype: String,
      recclass: String,
      reclat: Option[String] = None,
      reclong: Option[String] = None,
      year: Option[String] = None,
      `:@computed_region_cbhk_fwbd`: Option[String] = None,
      `:@computed_region_nnqa_25f4`: Option[String] = None
  )
}

object Movies {
  case class RootInterface(
      title: String,
      year: Int,
      cast: Seq[String],
      genres: Seq[String]
  )

}

object Reddit {

  case class Oembed(
      provider_url: String,
      title: String,
      html: String,
      thumbnail_width: Int,
      height: Int,
      width: Int,
      version: String,
      author_name: String,
      provider_name: String,
      thumbnail_url: String,
      `type`: String,
      thumbnail_height: Int,
      author_url: String
  )

  case class SecureMedia(
      `type`: String,
      oembed: Oembed
  )

  case class MediaEmbed(
      content: String,
      width: Int,
      scrolling: Boolean,
      media_domain_url: String,
      height: Int
  )

  case class Gildings(
      gid_1: Int,
      gid_2: Int,
      gid_3: Int
  )

  case class Data(
      approved_at_utc: Nullable[String],
      subreddit: String,
      selftext: String,
      author_fullname: String,
      saved: Boolean,
      mod_reason_title: Nullable[String],
      gilded: Int,
      clicked: Boolean,
      title: String,
      link_flair_richtext: Seq[String],
      subreddit_name_prefixed: String,
      hidden: Boolean,
      pwls: Int,
      link_flair_css_class: Nullable[String],
      downs: Int,
      parent_whitelist_status: String,
      hide_score: Boolean,
      name: Option[String] = None,
      quarantine: Option[Boolean] = None,
      link_flair_text_color: String,
      author_flair_background_color: Nullable[String],
      subreddit_type: String,
      ups: Int,
      domain: String,
      media_embed: MediaEmbed,
      author_flair_template_id: Nullable[String],
      is_original_content: Boolean,
      user_reports: Seq[String],
      secure_media: Nullable[Option[SecureMedia]],
      is_reddit_media_domain: Boolean,
      is_meta: Boolean,
      category: Nullable[String],
      secure_media_embed: Nullable[Option[MediaEmbed]],
      link_flair_text: Nullable[String],
      can_mod_post: Boolean,
      score: Int,
      approved_by: Nullable[String],
      thumbnail: String,
      edited: Boolean,
      author_flair_css_class: Nullable[String],
      author_flair_richtext: Seq[String],
      gildings: Gildings,
      content_categories: Nullable[String],
      is_self: Boolean,
      mod_note: Nullable[String],
      created: Double,
      link_flair_type: String,
      wls: Int,
      banned_by: Nullable[String],
      author_flair_type: String,
      contest_mode: Boolean,
      selftext_html: Nullable[String],
      likes: Nullable[String],
      suggested_sort: Nullable[String],
      banned_at_utc: Nullable[String],
      view_count: Nullable[String],
      archived: Boolean,
      no_follow: Boolean,
      is_crosspostable: Boolean,
      pinned: Boolean,
      over_18: Boolean,
      media_only: Boolean,
      can_gild: Boolean,
      spoiler: Boolean,
      locked: Boolean,
      author_flair_text: Nullable[String],
      visited: Boolean,
      num_reports: Nullable[String],
      distinguished: Nullable[String],
      subreddit_id: String,
      mod_reason_by: Nullable[String],
      removal_reason: Nullable[String],
      link_flair_background_color: String,
      id: String,
      is_robot_indexable: Boolean,
      report_reasons: Nullable[String],
      author: String,
      num_crossposts: Int,
      num_comments: Int,
      send_replies: Boolean,
      mod_reports: Seq[String],
      author_patreon_flair: Boolean,
      author_flair_text_color: Nullable[String],
      permalink: String,
      whitelist_status: String,
      stickied: Boolean,
      url: String,
      subreddit_subscribers: Int,
      created_utc: Double,
      media: Nullable[String],
      is_video: Boolean
  )

  case class Child(
      kind: String,
      data: Data
  )

  case class Data0(
      modhash: String,
      dist: Int,
      children: Seq[Child],
      after: String,
      before: Null
  )

  case class RootInterface(
      kind: String,
      data: Data0
  )
}

object RickMorty {
  case class Country(
      name: String,
      code: String,
      timezone: String
  )

  case class Embedded(
      episodes: Seq[Episodes]
  )

  case class Episodes(
      id: Int,
      url: String,
      name: String,
      season: Int,
      number: Int,
      airdate: String,
      airtime: String,
      airstamp: String,
      runtime: Int,
      image: Image,
      summary: String,
      _links: Links1
  )

  case class Externals(
      tvrage: Int,
      thetvdb: Int,
      imdb: String
  )

  case class Image(
      medium: String,
      original: String
  )

  case class Links(
      self: Self,
      previousepisode: Self
  )

  case class Links1(
      self: Self
  )

  case class Network(
      id: Int,
      name: String,
      country: Country
  )

  case class Rating(
      average: Double
  )

  case class RootInterface(
      id: Int,
      url: String,
      name: String,
      `type`: String,
      language: String,
      genres: Seq[String],
      status: String,
      runtime: Int,
      premiered: String,
      officialSite: String,
      schedule: Schedule,
      rating: Rating,
      weight: Int,
      network: Network,
      webChannel: Nullable[String],
      externals: Externals,
      image: Image,
      summary: String,
      updated: Int,
      _links: Links,
      _embedded: Embedded
  )

  case class Schedule(
      time: String,
      days: Seq[String]
  )

  case class Self(
      href: String
  )
}

object TempAnomaly {

  case class Description(
      title: String,
      units: String,
      base_period: String,
      missing: Int
  )

  case class RootInterface(
      description: Description,
      data: Map[String, Float]
  )
}

object ThaiCinemas {

  case class Group(
      code: String,
      english: String,
      thai: String,
      website: String
  )

  case class Results(
      url: String,
      group: Group,
      showtimes: String,
      num_views: Int,
      today_screens: Int,
      min_screens: Int,
      fav: Boolean,
      stars: Int,
      id: Int,
      code: String,
      slug: String,
      english: String,
      thai: String,
      website: String,
      location: String,
      point: String,
      tel: String
  )

  case class RootInterface(
      count: Int,
      next: String,
      previous: Nullable[String],
      results: Seq[Results]
  )
}

object Turkish {
  case class Event(
      date: String,
      description: String,
      lang: String,
      granularity: String,
      category1: Option[String] = None,
      category2: Option[String] = None
  )

  case class Result(count: String, events: Seq[Event])

  case class RootInterface(result: Result)

}

object TwitterApiResponse {

  case class Entities(
      user_mentions: Seq[UserMentions],
      urls: List[Urls] = Nil
  )

  case class Entities1(
      url: Url,
      description: Url
  )

  case class RetweetedStatus(
      created_at: String,
      id: Long,
      id_str: String,
      text: String,
      truncated: Boolean,
      entities: Url,
      source: String,
      user: User,
      is_quote_status: Boolean,
      retweet_count: Int,
      favorite_count: Int,
      favorited: Boolean,
      retweeted: Boolean,
      possibly_sensitive: Boolean,
      lang: String
  )

  case class RootInterface(
      created_at: String,
      id: Long,
      id_str: String,
      text: String,
      truncated: Boolean,
      entities: Entities,
      source: String,
      user: User,
      retweeted_status: RetweetedStatus,
      is_quote_status: Boolean,
      retweet_count: Int,
      favorite_count: Int,
      favorited: Boolean,
      retweeted: Boolean,
      possibly_sensitive: Boolean,
      lang: String
  )

  case class Url(
      urls: List[Urls] = Nil
  )

  case class Urls(
      url: String,
      expanded_url: String,
      display_url: String,
      indices: Seq[Int]
  )

  case class User(
      id: Long,
      id_str: String,
      name: String,
      screen_name: String,
      location: String,
      description: String,
      url: String,
      entities: Entities1,
      `protected`: Boolean,
      followers_count: Int,
      friends_count: Int,
      listed_count: Int,
      created_at: String,
      favourites_count: Int,
      utc_offset: Int,
      time_zone: String,
      geo_enabled: Boolean,
      verified: Boolean,
      statuses_count: Int,
      lang: String,
      contributors_enabled: Boolean,
      is_translator: Boolean,
      is_translation_enabled: Boolean,
      profile_background_color: String,
      profile_background_image_url: String,
      profile_background_image_url_https: String,
      profile_background_tile: Boolean,
      profile_image_url: String,
      profile_image_url_https: String,
      profile_banner_url: String,
      profile_link_color: String,
      profile_sidebar_border_color: String,
      profile_sidebar_fill_color: String,
      profile_text_color: String,
      profile_use_background_image: Boolean,
      has_extended_profile: Boolean,
      default_profile: Boolean,
      default_profile_image: Boolean,
      following: Boolean,
      follow_request_sent: Boolean,
      notifications: Boolean,
      translator_type: String
  )

  case class UserMentions(
      screen_name: String,
      name: String,
      id: Long,
      id_str: String,
      indices: Seq[Int]
  )
}
