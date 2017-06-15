

create table analytics_event
  ( ctime timestamptz not null default now()
  , cid text not null
  , tag text not null
  , addr text not null
  );

create index analytics_event_addr on analytics_event(addr);
