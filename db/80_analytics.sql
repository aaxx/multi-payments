

create table analytics_event
  ( ctime timestamptz not null default now()
  , cid text not null
  , action text not null
  );

create index analytics_event_cid_ix on analytics_event(cid);
